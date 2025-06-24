with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Exact_AB;

package body Move_Book is

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
      First  : Natural := 1;
      Last   : Natural := Natural (Game_Book.Length);
      Middle : Natural;
   begin
      if Last = 0 then
         return Option_Winner_Type'(False, 0);
      end if;
      loop
         Middle := (First + Last) / 2;
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

   procedure Missing_Move_Insert (b : Compressed_Board) is
   begin
      Ada.Text_IO.Put_Line (Out_Work_File, b'Image);
   end Missing_Move_Insert;

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

   procedure Add_Move (b : Game_State_Type; depth : Natural) is
      cb  : constant Compressed_Board := Compress (b);
      wt : Option_Winner_Type;
   begin
      if Get_Score (cb).Found then
         return;
      end if;

      wt := Exact_AB.Player_Search (b, depth);
      if wt.Found then
         declare
            mtl : constant String := To_Move_Table_Line (cb, wt.Winner, false);
         begin
            Ada.Text_IO.Put_Line (Update_File, mtl);
            Ada.Text_IO.Put_Line (mtl);
         end;
      else
         Missing_Move_Insert (cb);
         --Ada.Text_IO.Put_Line
         --  (cb'Image & " Failed " & To_String (Decompress (cb)));
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
