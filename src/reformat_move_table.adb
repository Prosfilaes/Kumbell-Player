pragma Restrictions (No_Obsolescent_Features);

with Ada.Text_IO;
with Move_Book;
with Ada.Command_Line;

procedure Reformat_Move_Table is
   out_work_file  : Ada.Text_IO.File_Type;
begin
   if Ada.Command_Line.Argument_Count /= 2 then
      Ada.Text_IO.Put_Line
        ("Proper format for calling " & Ada.Command_Line.Command_Name);
      Ada.Text_IO.Put_Line
        (Ada.Command_Line.Command_Name
         & " <move book> <out move book>");
      return;
   end if;
      Move_Book.Load_Book (Ada.Command_Line.Argument (1));
      Ada.Text_IO.Create
        (out_work_file, Ada.Text_IO.Out_File, Ada.Command_Line.Argument (2));
      Move_Book.Dump_Move_Book (out_work_file);
      
end Reformat_Move_Table;
