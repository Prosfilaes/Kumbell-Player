pragma Restrictions (No_Obsolescent_Features);

with Ada.Text_IO;
with Board; use Board;
with Move_Book;
with Ada.Command_Line;

procedure Build_From_Start is
   b             : constant Board.Game_State_Type := Board.Initialize;
   in_work_file  : Ada.Text_IO.File_Type;

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
         & " --base <move book> <out move book> <out work>");
      return;
   end if;
   if Ada.Command_Line.Argument (1) = "--base" then
      Move_Book.Load_Book (Ada.Command_Line.Argument (2), Ada.Command_Line.Argument (3));
      Ada.Text_IO.Put_Line
        ("* Loaded book and starting " & Board.To_String(b));
      Move_Book.Add_Move (b, 1);
      Move_Book.Add_Missing (1, Ada.Command_Line.Argument (4));

   else
      Move_Book.Load_Book (Ada.Command_Line.Argument (1), Ada.Command_Line.Argument (2));
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
               Move_Book.Add_Move (Board.Decompress (cb), 1);
            end if;
         end;
      end loop;
      Move_Book.Add_Missing (1, Ada.Command_Line.Argument (4));
   end if;

end Build_From_Start;
