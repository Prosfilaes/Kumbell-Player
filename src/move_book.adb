with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Text_IO;
with Exact_AB;
with Interfaces; use Interfaces;

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

   type Move_Book_Record is record
      start_cb : Compressed_Board;
      end_cb   : Compressed_Board;
      winner   : Winner_Type;
   end record;

   package Move_Book_List is new
     Ada.Containers.Vectors (Natural, Move_Book_Record);

   Game_Book : Move_Book_List.Vector;

   Update_File        : Ada.Text_IO.File_Type;
   Update_File_Open   : Boolean := false;
   Out_Work_File      : Ada.Text_IO.File_Type;
   Out_Work_File_Open : Boolean := false;

   procedure Load_Book
     (infilename      : String;
      outfilename     : String := "";
      outworkfilename : String := "") is
   begin
      declare
         File : Ada.Text_IO.File_Type;
      begin
         Ada.Text_IO.Open (File, Ada.Text_IO.In_File, infilename);
         while not Ada.Text_IO.End_Of_File (File) loop
            declare
               Line         : constant string := Ada.Text_IO.Get_Line (File);
               mbr          : Move_Book_Record;
               first_space  : Integer;
               second_space : Integer;
               winner_read  : Integer;
            begin
               -- Ignore all lines that don't start with a positive number
               if Line'Length > 10
                 and then Line (1) >= '1'
                 and then Line (1) <= '9'
               then
                  first_space := 2;
                  while Line (first_space) >= '0'
                    and then Line (first_space) <= '9'
                  loop
                     first_space := @ + 1;
                  end loop;
                  mbr.start_cb :=
                    Compressed_Board'Value (Line (1 .. first_space - 1));
                  second_space := first_space + 1;
                  if Line (first_space) = '-' then
                     while Line (second_space) >= '0'
                       and then Line (second_space) <= '9'
                     loop
                        second_space := @ + 1;
                     end loop;
                     mbr.end_cb :=
                       Compressed_Board'Value
                         (Line (first_space + 1 .. second_space - 1));
                  else
                     mbr.end_cb := mbr.start_cb;
                  end if;
                  winner_read :=
                    Integer'Value (Line (second_space .. Line'Last));
                  if winner_read = 0 then
                     mbr.winner := 0;
                  elsif winner_read = 1 then
                     mbr.winner := -1;
                  elsif winner_read = 2 then
                     mbr.winner := 1;
                  else
                     raise Constraint_Error;
                  end if;
                  Game_Book.Append (mbr);
               end if;
            end;
         end loop;
         Ada.Text_IO.Close (File);

      end;
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

   function Get_Score (b : Compressed_Board) return Option_Winner_Type is
      First  : Natural := 0;
      Last   : Natural := Natural (Game_Book.Length);
      Middle : Natural;
   begin
      if Game_Book_Map.Contains (b) then
         return Option_Winner_Type'(True, Game_Book_Map.Element (b));
      end if;
      if Last = 0 then -- if Game_Book is empty
         return Option_Winner_Type'(False, 0);
      else
         Last := @ - 1;
      end if;
      loop
         Middle := First + (Last - First) / 2;
         -- Loop invariant: First <= Middle <= Last
         if Game_Book (Middle).start_cb <= b
           and then Game_Book (Middle).end_cb >= b
         then
            return Option_Winner_Type'(True, Game_Book (Middle).winner);
         elsif Game_Book (Middle).start_cb > b then
            -- Last can't equal Middle, since / rounds down or towards zero
            Last := Middle;
         elsif Game_Book (Middle).end_cb < b then
            -- If Last - First > 1, then Middle >= First + 1.
            -- If Last = First or Last = First + 1 then Middle := First
            -- which will only happen if it's the first time through the
            -- loop and then will be caught below.
            First := Middle;
         end if;
         if First = Last then
            return Option_Winner_Type'(False, 0);
         end if;
         if First + 1 = Last then
            if Game_Book (First).start_cb <= b
              and then Game_Book (First).end_cb >= b
            then
               return Option_Winner_Type'(True, Game_Book (First).winner);
            elsif Game_Book (Last).start_cb <= b
              and then Game_Book (Last).end_cb >= b
            then
               return Option_Winner_Type'(True, Game_Book (Last).winner);
            else
               return Option_Winner_Type'(False, 0);
            end if;
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
      if Max_Heap_Count mod Unsigned_64(Max_Heap_Size / 4) = 0 then
         Move_Heap_P.Compact (Missing_Move_Heap, Max_Heap_Size);
      end if;
   end Missing_Move_Insert;

   procedure Reset_Missing_Move_Heap is
      new_heap: Move_Heap_P.Max_Heap_Type;
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
            mtl : constant String := To_Move_Table_Line (cb, wt.Winner, true);
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
