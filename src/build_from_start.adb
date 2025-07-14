pragma Restrictions (No_Obsolescent_Features);

with Ada.Text_IO; use Ada.Text_IO;
with Board;       use Board;
with Move_Book;   use Move_Book;
with Ada.Command_Line;

procedure Build_From_Start is
   cb          : Board.Compressed_Board;
   curr_moves  : Move_Heap_P.Max_Heap_Type;
   cycle_count : Natural := 0;
begin
   if not (Ada.Command_Line.Argument_Count = 2)
     and not (Ada.Command_Line.Argument_Count = 3
              and then Ada.Command_Line.Argument (1) = "--endgame")
   then
      Put_Line ("Proper formats for calling " & Ada.Command_Line.Command_Name);
      Put_Line
        (Ada.Command_Line.Command_Name & " <move book> <out move book>");
      Put_Line
        (Ada.Command_Line.Command_Name
         & " --endgame <move book> <out move book>");
      return;
   end if;
   Move_Book.Set_Max_Heap_Size (50_000_000);
   if Ada.Command_Line.Argument (1) = "--endgame" then
      Move_Book.Load_Book
        (Ada.Command_Line.Argument (2), Ada.Command_Line.Argument (3));
      Put_Line ("* Loaded book and starting building end game tables");
      Base_Boards (16, Add_Move'Access);
      Put_Line
        (Move_Book.Get_Missing_Move_Heap.Size'Image
         & " boards in missing move table.");
   else
      Move_Book.Load_Book
        (Ada.Command_Line.Argument (1), Ada.Command_Line.Argument (2));
      Put_Line
        ("* Loaded book and starting " & Board.To_String (Board.Initialize));
      Move_Book.Add_Move (Board.Initialize);
   end if;
   curr_moves := Move_Book.Get_Missing_Move_Heap;
   Move_Book.Reset_Missing_Move_Heap;
   while (curr_moves.size > 0) loop
      cycle_count := @ + 1;
      if Move_Book.Live_Book_Size > 250_000_000 then
         Put_Line
           ("The live book size exceeds 250 million. Exiting due to excess memory load.");
           return;
      end if;
      Put_Line
        ("** Cycle "
         & cycle_count'Image
         & " working on "
         & curr_moves.size'Image
         & " boards with "
         & Move_Book.Live_Book_Size'Image
         & " new boards solved.**");
      while (curr_moves.size > 0) loop
         Move_Heap_P.Pop_Max (curr_moves, cb);
         Move_Book.Add_Move (Decompress (cb));
      end loop;
      if cycle_count > 150 then
         exit;
      end if;
      curr_moves := Move_Book.Get_Missing_Move_Heap;
      Move_Book.Reset_Missing_Move_Heap;
   end loop;

   Move_Book.Close;
end Build_From_Start;
