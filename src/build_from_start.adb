pragma Restrictions (No_Obsolescent_Features);

with Ada.Text_IO;
with Board; use Board;
with Move_Book; use Move_Book;
with Ada.Command_Line;

procedure Build_From_Start is
   cb             : Board.Compressed_Board;
   in_work_file  : Ada.Text_IO.File_Type;
   depth : constant := 1;
   curr_moves : Move_Heap_P.Max_Heap_Type;
   cycle_count : Natural := 0;
   package CB_IO is new Ada.Text_IO.Integer_IO (Board.Compressed_Board);
begin
   if Ada.Command_Line.Argument_Count /= 2 then
      Ada.Text_IO.Put_Line
        ("Proper format for calling " & Ada.Command_Line.Command_Name);
      Ada.Text_IO.Put_Line
        (Ada.Command_Line.Command_Name
         & " <move book> <out move book>");
      return;
   end if;
   Move_Book.Load_Book (Ada.Command_Line.Argument (1), Ada.Command_Line.Argument (2));
   Ada.Text_IO.Put_Line
     ("* Loaded book and starting " & Board.To_String(Board.Initialize));
   Move_Book.Add_Move (Board.Initialize, depth);
   curr_moves := Move_Book.Get_Missing_Move_Heap;
   Move_Book.Reset_Missing_Move_Heap;
   while (curr_moves.Size > 0) loop
      cycle_count := @ + 1;
      Ada.Text_IO.Put_Line ("** Cycle " & cycle_count'Image & " working on " & curr_moves.size'Image & " items.**");
      while (curr_moves.Size > 0) loop
         Move_Heap_P.Pop_Max (curr_moves, cb);
         Move_Book.Add_Move (Decompress(cb), depth);
      end loop;
      curr_moves := Move_Book.Get_Missing_Move_Heap;
      Move_Book.Reset_Missing_Move_Heap;
   end loop;
   Move_Book.Close;
end Build_From_Start;
