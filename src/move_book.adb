with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Text_IO;
with Exact_AB;
with Interfaces;     use Interfaces;
with Interfaces.C;   use Interfaces.C;
with Mmap;

package body Move_Book is

   function Is_Equal (a, b : Compressed_Board) return Boolean with Inline is
   begin
      return a = b;
   end Is_Equal;

   function SplitMix64_Hash
     (X : Compressed_Board) return Ada.Containers.Hash_Type
   is
      Y : Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (X);
   begin
      Y := Y xor (Y / 2**30);
      Y := Y * 16#BF58476D1CE4E5B9#;
      Y := Y xor (Y / 2**27);
      Y := Y * 16#94D049BB133111EB#;
      Y := Y xor (Y / 2**31);
      return Ada.Containers.Hash_Type (Y / 2**32);  -- Top 32 bits
   end SplitMix64_Hash;

   package Move_Hash_Map is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => Compressed_Board,
        Element_Type    => Winner_Type,
        Hash            => SplitMix64_Hash,
        Equivalent_Keys => Is_Equal);

   Game_Book_Map : Move_Hash_Map.Map;

   In_File_Open       : Boolean := false;
   Update_File        : Ada.Text_IO.File_Type;
   Update_File_Open   : Boolean := false;
   Out_Work_File      : Ada.Text_IO.File_Type;
   Out_Work_File_Open : Boolean := false;

   type In_Memory_Table_Record is record 
      loc : Long_Integer;
      extents : Mmap.Mmap_Record;
   end record;

   package In_Memory_Table_Type is new Ada.Containers.Vectors (Natural, In_Memory_Table_Record);
   In_Memory_Table : In_Memory_Table_Type.Vector;
   use In_Memory_Table_Type;

   procedure Build_In_Memory_Table with Pre => In_File_Open is
      sample : In_Memory_Table_Record;
      length : constant Long_Integer := Mmap.Length;
      table_length : constant := 128;
   begin
      sample.extents := Mmap.Get (0);
      sample.loc := 0;
      Append (In_Memory_Table, sample);
      for i in 0 .. table_length - 1 loop
         declare
            -- table_length * length needs to be in Long_Integer
            -- length is limited by drive space, so a trillion ~ 2**40
            -- is about tops now, thus as long as table_length is less
            -- than 2**24, this should be safe
            loc : constant Long_Integer := (Long_Integer(i) * length) / table_length;
         begin
            sample.extents := Mmap.Get (loc);
            sample.loc := loc;
            Append (In_Memory_Table, sample);
         end;
      end loop;
      sample.extents := Mmap.Get (length - 1);
      sample.loc := length - 1;
      Append (In_Memory_Table, sample);
   end Build_In_Memory_Table;

   procedure Load_Book
     (infilename      : String := "";
      outfilename     : String := "";
      outworkfilename : String := "") is
   begin
      if infilename /= "" then
         In_File_Open := True;
         Mmap.Open (infilename);
         -- Build_In_Memory_Table;
      end if;
      if outfilename /= "" then
         Update_File_Open := True;
         Ada.Text_IO.Create (Update_File, Ada.Text_IO.Out_File, outfilename);
      end if;
      if outworkfilename /= "" then
         Out_Work_File_Open := True;
         Ada.Text_IO.Create
           (Out_Work_File, Ada.Text_IO.Out_File, outworkfilename);
      end if;
      return;
   end Load_Book;

   function To_Winner (uc : unsigned_char) return Winner_Type with Inline is
   begin
      if uc = 0 then
         return 0;
      elsif uc = 1 then
         return -1;
      elsif uc = 2 then
         return 1;
      end if;
      raise Program_Error;
   end To_Winner;

   function Get_Score (b : Compressed_Board) return Option_Winner_Type is
      First  : Long_Integer := 0;
      Last   : Long_Integer := Mmap.Length - 1;
      Middle : Long_Integer;
      Data   : Mmap.Mmap_Record;
   begin
      if Game_Book_Map.Contains (b) then
         return Option_Winner_Type'(True, Game_Book_Map.Element (b));
      end if;
      if not In_File_Open then
         return Option_Winner_Type'(False, 0);
      end if;
      loop
         -- / rounds down or towards zero
         Middle := First + (Last - First) / 2;
         -- Loop invariant: First <= Middle <= Last
         Data := Mmap.Get (Middle);
         if Compressed_Board (Data.start_idx) <= b
           and then Compressed_Board (Data.end_idx) >= b
         then
            return Option_Winner_Type'(True, To_Winner (Data.Value));
         elsif Compressed_Board (Data.start_idx) > b then
            Last := Middle - 1;
         elsif Compressed_Board (Data.end_idx) < b then
            -- If Last - First > 1, then Middle >= First + 1.
            -- If Last = First or Last = First + 1 then Middle := First
            -- which will only happen if it's the first time through the
            -- loop and then will be caught below.
            First := Middle + 1;
         end if;
         if First > Last then
            return Option_Winner_Type'(False, 0);
         end if;
      end loop;
   end Get_Score;

   Max_Heap_Size     : Natural := 50_000_000;
   Max_Heap_Count    : Unsigned_64 := 0;
   Missing_Move_Heap : Move_Heap_P.Max_Heap_Type;

   procedure Set_Max_Heap_Size (size : in Natural) is
   begin
      Max_Heap_Size := size;
   end Set_Max_Heap_Size;

   function Get_Missing_Move_Heap return Move_Heap_P.Max_Heap_Type is
   begin
      Missing_Move_Heap.Compact (Max_Heap_Size);
      return Missing_Move_Heap;
   end Get_Missing_Move_Heap;

   procedure Missing_Move_Insert (b : Compressed_Board) is
   begin
      Max_Heap_Count := @ + 1;
      Move_Heap_P.Insert (Missing_Move_Heap, b);
      if Max_Heap_Count mod Unsigned_64 (Max_Heap_Size / 4) = 0 then
         Move_Heap_P.Compact (Missing_Move_Heap, Max_Heap_Size);
      end if;
   end Missing_Move_Insert;

   procedure Reset_Missing_Move_Heap is
      new_heap : Move_Heap_P.Max_Heap_Type;
   begin
      Missing_Move_Heap := new_heap;
      Max_Heap_Count := 0;
   end Reset_Missing_Move_Heap;

   function To_Move_Table_Line
     (cb : Compressed_Board; score : Winner_Type; terse : Boolean := False)
      return String is
   begin
      return
        (cb'Image
         & (if score = -1
            then " 1 "
            elsif score = 1
            then " 2 "
            elsif score = 0
            then " 0 "
            else "xxx")
         & (if terse then "" else " " & To_String (Decompress (cb))));
   end To_Move_Table_Line;

   procedure Add_Move (b : Game_State_Type) is
      cb : constant Compressed_Board := Compress (b);
      wt : Option_Winner_Type;
   begin
      if Get_Score (cb).Found then
         return;
      end if;

      wt := Exact_AB.Player_Search (b, 1);
      if wt.Found then
         declare
            mtl : constant String := To_Move_Table_Line (cb, wt.Winner, false);
         begin
            Ada.Text_IO.Put_Line (Update_File, mtl);
            --Ada.Text_IO.Put_Line (mtl);
            Game_Book_Map.Insert (cb, wt.Winner);
         end;
      else
         Missing_Move_Insert (cb);
         --Ada.Text_IO.Put_Line
         --    (cb'Image & " Failed " & To_String (Decompress (cb)));
      end if;
   end Add_Move;

   procedure Close is
   begin
      if In_File_Open then
         Mmap.Close;
         In_File_Open := false;
      end if;
      if Update_File_Open then
         Ada.Text_IO.Close (Update_File);
         Update_File_Open := false;
      end if;
      if Out_Work_File_Open then
         Ada.Text_IO.Close (Out_Work_File);
         Out_Work_File_Open := false;
      end if;
   end Close;

end Move_Book;
