with Ada.Integer_Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Containers;        use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Exact_AB;

package body Move_Book is
   function Hash (b : Game_State_Type) return Ada.Containers.Hash_Type is
      type Board_Bytes is mod 2**64;
      cb : constant Board_Bytes := Board_Bytes (Compress (b));
   begin
      return Ada.Containers.Hash_Type (cb mod 2**32 xor (cb / 2**32));
   end Hash;

   function Is_Equal (a, b : Game_State_Type) return Boolean is
   begin
      return a = b;
   end Is_Equal;

   package Move_Hash_Map is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => Game_State_Type,
        Element_Type    => Move_Score_Type,
        Hash            => Hash,
        Equivalent_Keys => Is_Equal);

   Game_Book : Move_Hash_Map.Map;

   package Move_Hash_Set is new
     Ada.Containers.Hashed_Sets
       (Element_Type        => Game_State_Type,
        Equivalent_Elements => Is_Equal,
        Hash                => Hash);

   Unknown_Move_Book : Move_Hash_Set.Set;

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
      while (end_pos < s'Last and s (end_pos) /= ' ') loop
         end_pos := end_pos + 1;
      end loop;
      sms.move := Move_Type_from_String (s (start_pos + 1 .. end_pos));
      return sms;
   end Get_Move_Score_Type;

   procedure Load_Book (filename : String) is
   begin
      declare
         File : Ada.Text_IO.File_Type;
      begin
         Ada.Text_IO.Open (File, Ada.Text_IO.In_File, filename);
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
                  if not Game_Book.Contains (b) then
                     Game_Book.Insert (b, sms);
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
      return;
   end Load_Book;

   function Is_Book_Move (b : Game_State_Type) return Boolean is
   begin
      return Game_Book.Contains (b);
   end Is_Book_Move;

   function Get_Score (b : Game_State_Type) return Winner_Type is
   begin
      return Game_Book.Element (b).score;
   end Get_Score;

   function Get_Move (b : Game_State_Type) return Move_Score_Type is
   begin
      return Game_Book.Element (b);
   end Get_Move;

   procedure Missing_Move_Insert (b : Game_State_Type) is
   begin
      if not Unknown_Move_Book.Contains (b) then
         Unknown_Move_Book.Insert (b);
      end if;
   end Missing_Move_Insert;

   procedure Add_Move (b : Game_State_Type; depth : Natural) is
      cb  : constant Compressed_Board := Compress (b);
      sms : Move_Score_Type;
   begin
      if Move_Book.Is_Book_Move (b) then
         return;
      end if;

      sms := Exact_AB.Best_Move (b, depth);
      if sms.exact then
         Game_Book.Insert (b, sms);
      else
         Missing_Move_Insert (b);
         Ada.Text_IO.Put_Line
           ("*** Didn't conclude -" & cb'Image & " " & To_String (b));
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
            b            : constant Game_State_Type :=
              Move_Hash_Map.Key (b_sms);
            sms          : Move_Score_Type;
            cb           : Compressed_Board;
         begin
            sms := Move_Hash_Map.Element (b_sms);
            cb := Compress (b);
            if not board_found then
               board_found := True;
               Ada.Text_IO.Put_Line (f, "");
               Ada.Text_IO.Put_Line (f, "== " & cat_title & " ==");
            end if;
            if sms.score = -1 then
               board_list.Append
                 (To_Unbounded_String
                    (cb'Image
                     & " 1 "
                     & Move_Type'Image (sms.move)
                     & " "
                     & To_String (b)));
            elsif sms.score = 1 then
               board_list.Append
                 (To_Unbounded_String
                    (cb'Image
                     & " 2 "
                     & Move_Type'Image (sms.move)
                     & " "
                     & To_String (b)));
            elsif sms.score = 0 then
               board_list.Append
                 (To_Unbounded_String
                    (cb'Image
                     & " 0 "
                     & Move_Type'Image (sms.move)
                     & " "
                     & To_String (b)));
            else
               raise Constraint_Error;
            end if;
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
            b        : constant Game_State_Type := Move_Hash_Map.Key (b_sms);
            category : constant Board_Categories_Type := Categorize (b);
            sms      : constant Move_Score_Type :=
              Move_Hash_Map.Element (b_sms);
         begin
            Per_Size_Maps (category).Insert (b, sms);
         end;
      end loop;

      for cat in Board_Categories_Type'Range loop
         if not Per_Size_Maps (cat).Is_Empty then
            Dump_Move_Book_Local (f, Per_Size_Maps (cat), Title_Line (cat));
         end if;
      end loop;
   end Dump_Move_Book;

   procedure Add_Missing (depth : Natural) is
      use Move_Hash_Set;
      count       : Ada.Containers.Count_Type := 0;
      count_old : Ada.Containers.Count_Type := 0;
      iterations  : Integer := 0;
      current_set : Move_Hash_Set.Set;
   begin
      while Unknown_Move_Book.Length > 0 loop
         iterations := @ + 1;
         current_set := Unknown_Move_Book;
         count_old := Unknown_Move_Book.Length;
         Unknown_Move_Book := Empty_Set;
         count := current_set.Length;
         pragma Assert (count = count_old);
         Ada.Text_IO.Put_Line
           ("** Iteration "
            & iterations'Image
            & " Missing boards: "
            & count'Image);
         for b of current_set loop
            --if count mod 1_000_000 = 0 then
            Ada.Text_IO.Put_Line
              ("*** Iteration "
               & iterations'Image
               & " Boards left: "
               & count'Image
               & " "
               & To_String (b));
            --end if;
            Add_Move (b, depth * iterations);
            count := @ - 1;
         end loop;
      end loop;
   end Add_Missing;

end Move_Book;
