with Ada.Integer_Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Containers;        use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Exact_AB;
with Interfaces; use Interfaces;

package body Move_Book is

   function Is_Equal (a, b : Compressed_Board) return Boolean is
   begin
      return a = b;
   end Is_Equal;

   function SplitMix64_Hash (X : Compressed_Board) return Ada.Containers.Hash_Type is
      Y : Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (X);
   begin
      Y := Y xor (Y / 2 ** 30);
      Y := Y * 16#BF58476D1CE4E5B9#;
      Y := Y xor (Y / 2 ** 27);
      Y := Y * 16#94D049BB133111EB#;
      Y := Y xor (Y / 2 ** 31);
      return Ada.Containers.Hash_Type (Y / 2 ** 32);  -- Top 32 bits
   end SplitMix64_Hash;

   package Move_Hash_Map is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => Compressed_Board,
        Element_Type    => Move_Score_Type,
        Hash            => SplitMix64_Hash,
        Equivalent_Keys => Is_Equal);

   Game_Book : Move_Hash_Map.Map;

   function Get_Move_Score_Type (s : String) return Move_Score_Type is
      sms       : Move_Score_Type;
      start_pos : Positive := 1;
      end_pos   : Positive;
      i         : Integer;
   begin
      Ada.Integer_Text_IO.Get (s, i, start_pos);
      if i = 0 then
         sms.Score := 0;
      elsif i = 1 then
         sms.Score := -1;
      elsif i = 2 then
         sms.Score := 1;
      else
         raise Constraint_Error with "Invalid score in Get_Move_Score_Type";
      end if;
      end_pos := start_pos + 1;
      while (end_pos < s'Last and s (end_pos) = ' ') loop
         end_pos := end_pos + 1;
      end loop;
      while (end_pos < s'Last and s (end_pos) /= ' ') loop
         end_pos := end_pos + 1;
      end loop;
      sms.move := Move_Type_from_String (s (start_pos + 1 .. end_pos));
      sms.Exact := true;
      return sms;
   end Get_Move_Score_Type;

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
               Line        : constant string := Ada.Text_IO.Get_Line (File);
               cb          : Compressed_Board;
               sms         : Move_Score_Type;
               b           : Game_State_Type;
               first_space : Integer;
            begin
               -- Ignore all lines that don't start with a positive number
               if Line'Length > 16
                 and then Line (1) = ' '
                 and then Line (2) >= '1'
                 and then Line (2) <= '9'
               then
                  first_space := 2;
                  while Line (first_space) >= '0'
                    and then Line (first_space) <= '9'
                  loop
                     first_space := @ + 1;
                  end loop;
                  cb := Compressed_Board'Value (Line (1 .. first_space - 1));
                  b := Decompress (cb);
                  pragma Assert (Compress (b) = cb);
                  sms :=
                    Get_Move_Score_Type (Line (first_space + 1 .. Line'Last));
                  if not Game_Book.Contains (cb) then
                     Game_Book.Insert (cb, sms);
                  else
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "Duplicate entry in book: "
                        & cb'Image
                        & " "
                        & Line (14 .. Line'Last));
                  end if;
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

   function Is_Book_Move (b : Compressed_Board) return Boolean is
   begin
      return Game_Book.Contains (b);
   end Is_Book_Move;

   function Get_Score (b : Compressed_Board) return Winner_Type is
   begin
      return Game_Book.Element (b).score;
   end Get_Score;

   function Get_Move (b : Compressed_Board) return Move_Score_Type is
   begin
      return Game_Book.Element (b);
   end Get_Move;

   procedure Missing_Move_Insert (b : Compressed_Board) is
   begin
      Ada.Text_IO.Put_Line (Out_Work_File, b'Image);
   end Missing_Move_Insert;

   function To_Move_Table_Line
     (cb : Compressed_Board; sms : Move_Score_Type) return String
   is
   begin
      return
        (cb'Image
         & (if sms.score = -1
            then " 1 "
            elsif sms.score = 1
            then " 2 "
            elsif sms.score = 0 then " 0 "
            else "xxx")
         & Move_Type'Image (sms.move)
         & " "
         & To_String (Decompress(cb)));
   end To_Move_Table_Line;

   procedure Add_Move (b : Game_State_Type; depth : Natural) is
      cb  : constant Compressed_Board := Compress (b);
      sms : Move_Score_Type;
   begin
      if Move_Book.Is_Book_Move (cb) then
         return;
      end if;

      sms := Exact_AB.Best_Move (b, depth);
      if sms.exact then
         Ada.Text_IO.Put_Line (Update_File, To_Move_Table_Line (cb, sms));
         Game_Book.Insert (cb, sms);
         Ada.Text_IO.Put_Line
           ("*** Solved "
            & sms.Score'Image
            & " -- "
            & cb'Image
            & " "
            & To_String (b));
      else
         Missing_Move_Insert (cb);
      end if;
   end Add_Move;

   procedure Dump_Move_Book_Local
     (f           : Ada.Text_IO.File_Type;
      per_cat_map : Move_Hash_Map.Map;
      cat_title   : String)
   is
      board_found : Boolean;
      package Board_Container is new
        Ada.Containers.Vectors (Natural, Unbounded_String);
      use Board_Container;
      package A_Sorter is new Generic_Sorting;
      board_list  : Vector;
   begin
      board_found := False;
      for b_sms in per_cat_map.Iterate loop
         declare
            cb   : constant Compressed_Board := Move_Hash_Map.Key (b_sms);
            sms : Move_Score_Type;
         begin
            sms := Move_Hash_Map.Element (b_sms);
            if not board_found then
               board_found := True;
               Ada.Text_IO.Put_Line (f, "");
               Ada.Text_IO.Put_Line (f, "== " & cat_title & " ==");
            end if;
            board_list.Append
              (To_Unbounded_String (To_Move_Table_Line (cb, sms)));
         end;
      end loop;

      if board_found then
         A_Sorter.Sort (board_list);
         for line of board_list loop
            Ada.Text_IO.Unbounded_IO.Put_Line (f, line);
         end loop;
      end if;
   end Dump_Move_Book_Local;

   procedure Dump_Move_Book (f : Ada.Text_IO.File_Type) is
      Per_Size_Maps : array (Board_Categories_Type) of Move_Hash_Map.Map;
   begin
      for b_sms in Game_Book.Iterate loop
         declare
            cb        : constant Compressed_Board := Move_Hash_Map.Key (b_sms);
            category : constant Board_Categories_Type := Categorize (Decompress(cb));
            sms      : constant Move_Score_Type :=
              Move_Hash_Map.Element (b_sms);
         begin
            Per_Size_Maps (category).Insert (cb, sms);
         end;
      end loop;

      for cat in Board_Categories_Type'Range loop
         if not Per_Size_Maps (cat).Is_Empty then
            Dump_Move_Book_Local (f, Per_Size_Maps (cat), Title_Line (cat));
         end if;
      end loop;
   end Dump_Move_Book;

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
