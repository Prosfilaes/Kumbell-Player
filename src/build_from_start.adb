pragma Restrictions (No_Obsolescent_Features);

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Long_Integer_Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Board;     use Board;
with Format_Number;
with Move_Book; use Move_Book;

procedure Build_From_Start is
   cb                : Board.Compressed_Board;
   curr_moves        : Move_Heap_P.Max_Heap_Type;
   cycle_count       : Natural := 0;
   max_heap_size     : constant := 150_000_000;
   mhs_name          : constant String := Format_Number (max_heap_size);
   live_book_size    : constant := 250_000_000;
   lbs_name          : constant String := Format_Number (live_book_size);
   overflow_filename : Unbounded_String;
begin
   if not (Ada.Command_Line.Argument_Count = 2)
     and not (Ada.Command_Line.Argument_Count = 3)
     and not ((Ada.Command_Line.Argument_Count = 4
               or Ada.Command_Line.Argument_Count = 5)
              and then Ada.Command_Line.Argument (1) = "--boardlist")
   then
      Put_Line ("Proper formats for calling " & Ada.Command_Line.Command_Name);
      Put_Line
        (Ada.Command_Line.Command_Name
         & " <move book> <out move book> (<overflow boards>)");
      Put_Line
        (Ada.Command_Line.Command_Name
         & " none <out move book> (<overflow boards>)");
      Put_Line
        (Ada.Command_Line.Command_Name
         & " --boardlist <board list> <move book> <out move book> (<overflow boards>)");
      Put_Line
        (Ada.Command_Line.Command_Name
         & " --endgame <move book> <out move book>");
      return;
   end if;
   Move_Book.Set_Max_Heap_Size (max_heap_size);
   if Ada.Command_Line.Argument (1) = "--endgame" then
      Move_Book.Load_Book
        (Ada.Command_Line.Argument (2), Ada.Command_Line.Argument (3));
      Put_Line ("* Loaded book and starting building end game tables");
      Base_Boards (16, Add_Move'Access);
      Put_Line
        (Move_Book.Get_Missing_Move_Heap.Size'Image
         & " boards in missing move table.");
   elsif Ada.Command_Line.Argument (1) = "--boardlist" then
      Put_Line
        ("* Running with a max heap size of "
         & mhs_name
         & " live book size of "
         & lbs_name
         & ".");
      if Ada.Command_Line.Argument_Count > 4 then
         overflow_filename :=
           To_Unbounded_String (Ada.Command_Line.Argument (5));
      end if;
      Move_Book.Load_Book
        (Ada.Command_Line.Argument (3), Ada.Command_Line.Argument (4));
      Put_Line ("* Loaded book and starting processing the board list");
      declare
         boardlist_file : Ada.Text_IO.File_Type;
         move           : Long_Integer;
      begin
         Ada.Text_IO.Open
           (boardlist_file,
            Ada.Text_IO.In_File,
            Ada.Command_Line.Argument (2));
         while not Ada.Text_IO.End_Of_File (boardlist_file) loop
            Ada.Long_Integer_Text_IO.Get (boardlist_file, move);
            Move_Book.Add_Move (Decompress (Compressed_Board (move)));
            -- Skip to next line, in case there's trailing whitespace
            if not Ada.Text_IO.End_Of_Line (boardlist_file) then
               Ada.Text_IO.Skip_Line (boardlist_file);
            end if;
         end loop;
         Ada.Text_IO.Close (boardlist_file);
      end;
   else
      Put_Line
        ("* Running with a max heap size of "
         & mhs_name
         & " live book size of "
         & lbs_name
         & ".");
      if Ada.Command_Line.Argument_Count > 2 then
         overflow_filename :=
           To_Unbounded_String (Ada.Command_Line.Argument (3));
      end if;
      if Ada.Command_Line.Argument (1) = "none" then
         Move_Book.Load_Book
            ("", Ada.Command_Line.Argument (2));
      else
         Move_Book.Load_Book
            (Ada.Command_Line.Argument (1), Ada.Command_Line.Argument (2));
      end if;
      Put_Line
        ("* Loaded book and starting " & Board.To_String (Board.Initialize));
      Move_Book.Add_Move (Board.Initialize);
   end if;
   curr_moves := Move_Book.Get_Missing_Move_Heap;
   Move_Book.Reset_Missing_Move_Heap;
   while (curr_moves.size > 0) loop
      cycle_count := @ + 1;
      if Move_Book.Live_Book_Size > live_book_size then
         declare
            dump_file : Ada.Text_IO.File_Type;
         begin
            Put_Line
              ("The live book size exceeds "
               & lbs_name
               & ". Exiting due to excess memory load.");
            if overflow_filename /= "" then
               Put_Line ("Dumping to " & To_String (overflow_filename));
               Ada.Text_IO.Create
                 (dump_file,
                  Ada.Text_IO.Out_File,
                  To_String (overflow_filename));
               while (curr_moves.size > 0) loop
                  Move_Heap_P.Pop_Max (curr_moves, cb);
                  Put_Line (dump_file, cb'Image);
               end loop;
            end if;
            return;
         end;
      end if;
      Put_Line
        ("** Cycle "
         & cycle_count'Image
         & " working on "
         & Format_Number(Long_Integer (curr_moves.size))
         & " boards with "
         & Format_Number (Long_Integer (Move_Book.Live_Book_Size))
         & " new boards solved.**");
      while (curr_moves.size > 0) loop
         Move_Heap_P.Pop_Max (curr_moves, cb);
         Move_Book.Add_Move (Decompress (cb));
      end loop;
      if cycle_count > 150 then
         Put_Line
           ("This program has exceeded the max cycle count and is exiting.");
         Put_Line
           ("If this is not indicative of an infinite loop, the cycle_count limit can be increased.");
         exit;
      end if;
      curr_moves := Move_Book.Get_Missing_Move_Heap;
      Move_Book.Reset_Missing_Move_Heap;
   end loop;

   Move_Book.Close;
end Build_From_Start;
