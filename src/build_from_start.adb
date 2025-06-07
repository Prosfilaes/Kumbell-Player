pragma Restrictions (No_Obsolescent_Features);

with Ada.Text_IO;
with Board; use Board;
with Move_Book;
with Ada.Command_Line;

procedure Build_From_Start is
   b             : constant Board.Game_State_Type := Board.Initialize;
   in_work_file  : Ada.Text_IO.File_Type;
   depth : constant := 1;

   package CB_IO is new Ada.Text_IO.Integer_IO (Board.Compressed_Board);
begin
   if Ada.Command_Line.Argument_Count /= 4 then
      Ada.Text_IO.Put_Line
        ("Proper formats for calling " & Ada.Command_Line.Command_Name);
      Ada.Text_IO.Put_Line
        (Ada.Command_Line.Command_Name
         & " <move book> <out move book> <in work> <out work>");
      Ada.Text_IO.Put_Line
        (Ada.Command_Line.Command_Name
         & " --start <move book> <out move book> <out work>");
      Ada.Text_IO.Put_Line
        (Ada.Command_Line.Command_Name
         & " --endgame <move book> <out move book> <out work>");
      return;
   end if;
   if Ada.Command_Line.Argument (1) = "--start" then
      Move_Book.Load_Book (Ada.Command_Line.Argument (2), Ada.Command_Line.Argument (3), Ada.Command_Line.Argument(4));
      Ada.Text_IO.Put_Line
        ("* Loaded book and starting " & Board.To_String(b));
      Move_Book.Add_Move (b, depth);
   elsif Ada.Command_Line.Argument (1) = "--endgame" then
      Move_Book.Load_Book (Ada.Command_Line.Argument (2), Ada.Command_Line.Argument (3), Ada.Command_Line.Argument(4));
      Ada.Text_IO.Put_Line
        ("* Loaded book and starting to build the endgame tables.");
      for b of Board.Base_Boards loop
         Move_Book.Add_Move (b, depth);
      end loop;
   else
      Move_Book.Load_Book (Ada.Command_Line.Argument (1), Ada.Command_Line.Argument (2), Ada.Command_Line.Argument (4));
      Ada.Text_IO.Put_Line
        ("* Loaded book and starting " & Ada.Command_Line.Argument (3));
      Ada.Text_IO.Open
        (in_work_file, Ada.Text_IO.In_File, Ada.Command_Line.Argument (3));
      while (not Ada.Text_IO.End_Of_File (in_work_file)) loop
         declare
            cb : Board.Compressed_Board;
         begin
            CB_IO.Get (in_work_file, cb);
            if cb = 0 then
               Ada.Text_IO.Put_Line ("* NULL cb detected");
            else
               Move_Book.Add_Move (Board.Decompress (cb), depth);
            end if;
         end;
      end loop;
   end if;
   Move_Book.Close;
end Build_From_Start;
