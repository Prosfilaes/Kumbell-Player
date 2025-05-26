with Ada.Integer_Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Exact_AB;

package body Move_Book is
   function Hash (b : Game_State) return Ada.Containers.Hash_Type is
      type Board_Bytes is mod 2**64;
      cb : constant Board_Bytes := Board_Bytes (Compress (b));
   begin
      return Ada.Containers.Hash_Type (cb mod 2**32 xor (cb / 2**32));
   end Hash;

   function Is_Equal (a, b : Game_State) return Boolean is
   begin
      return a = b;
   end Is_Equal;

   package Move_Hash_Map is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => Game_State,
        Element_Type    => Spot_Move_Score,
        Hash            => Hash,
        Equivalent_Keys => Is_Equal);

   Game_Book : Move_Hash_Map.Map;

   package Move_Hash_Set is new
     Ada.Containers.Hashed_Sets
       (Element_Type        => Game_State,
        Equivalent_Elements => Is_Equal,
        Hash            => Hash);


   package Move_Queue_Interface is new
     Ada.Containers.Synchronized_Queue_Interfaces (Game_State);
   package Move_Queue is new
     Ada.Containers.Unbounded_Synchronized_Queues (Move_Queue_Interface);
   Unknown_Move_Book : Move_Queue.Queue;

   function Get_Spot_Move_Score (s : String) return Spot_Move_Score is
      sms       : Spot_Move_Score;
      spot_move : Board_Spot;
      new_score : Score;
      start_pos : Positive := 1;
      i         : Integer;
   begin
      Ada.Integer_Text_IO.Get (s, i, start_pos);
      new_score := Score (i);
      Ada.Integer_Text_IO.Get (s (start_pos + 1 .. s'Last), i, start_pos);
      spot_move := Board_Spot (i);
      sms.move := spot_move;
      sms.exact := True;
      case new_score is
         when 1 =>
            sms.est_score := 127;

         when 2 =>
            sms.est_score := -127;

         when 0 =>
            sms.est_score := 0;

         when others =>
            raise Constraint_Error
              with "Invalid score value: " & new_score'Image;
      end case;
      return sms;
   end Get_Spot_Move_Score;

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
               sms         : Spot_Move_Score;
               b           : Game_State;
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
                  -- Ada.Text_IO.Put_Line (To_String(b));
                  -- if not Is_Legal_Board (b) then
                  --   Ada.Text_IO.Put_Line (Line);
                  --end if;
                  pragma Assert (Compress (b) = cb);
                  sms :=
                    Get_Spot_Move_Score (Line (first_space + 1 .. Line'Last));
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

   function Is_Book_Move (b : Game_State) return Boolean is
   begin
      return Game_Book.Contains (b);
   end Is_Book_Move;

   function Get_Score (b : Game_State) return Score is
   begin
      return Game_Book.Element (b).est_score;
   end Get_Score;

   function Get_Move (b : Game_State) return Spot_Move_Score is
   begin
      return Game_Book.Element (b);
   end Get_Move;

   procedure Missing_Move_Insert (b : Game_State) is
   begin
      Unknown_Move_Book.Enqueue (b);
   end Missing_Move_Insert;

   procedure Add_Move (b : Game_State; depth : Natural) is
      cb  : constant Compressed_Board := Compress (b);
      sms : Spot_Move_Score;
   begin
      if Move_Book.Is_Book_Move (b) then
         return;
      end if;

      sms := Exact_AB.Best_Move (b, depth);
      if sms.exact then
         Game_Book.Insert (b, sms);
         pragma Assert (if b.store (1) = 36 then sms.est_score /= -127);
      else
         Missing_Move_Insert (b);
         Ada.Text_IO.Put_Line
           ("*** Didn't conclude -" & cb'Image & " " & To_String (b));
      end if;
   end Add_Move;

   procedure Dump_Move_Book_Local
     (f               : Ada.Text_IO.File_Type;
      per_size_map    : Move_Hash_Map.Map;
      pieces_on_board : Piece_Count;
      Player1_Store   : Piece_Count)
   is

      board_found : Boolean;
      package Board_Container is new
        Ada.Containers.Vectors (Natural, Unbounded_String);
      use Board_Container;
      package A_Sorter is new Generic_Sorting;
      board_list  : Vector;
   begin
      board_found := False;
      for b_sms in per_size_map.Iterate loop
         declare
            b            : constant Game_State := Move_Hash_Map.Key (b_sms);
            board_pieces : constant Piece_Count :=
              Piece_Count (72 - Integer (b.store (1)) - Integer (b.store (2)));
            sms          : Spot_Move_Score;
            cb           : Compressed_Board;
         begin
            if board_pieces = pieces_on_board then
               if player1_store = b.store (1) then
                  sms := Move_Hash_Map.Element (b_sms);
                  cb := Compress (b);
                  if not board_found then
                     board_found := True;
                     Ada.Text_IO.Put_Line (f, "");
                     Ada.Text_IO.Put_Line
                       (f,
                        "== "
                        & pieces_on_board'Image
                        & " pieces on the board and "
                        & b.store (1)'Image
                        & " vs "
                        & b.store (2)'Image
                        & " ==");
                  end if;
                  if sms.est_score = 127 then
                     board_list.Append
                       (To_Unbounded_String
                          (cb'Image
                           & " 1 "
                           & Board_Spot'Image (sms.move)
                           & " "
                           & To_String (b)));
                  elsif sms.est_score = -127 then
                     board_list.Append
                       (To_Unbounded_String
                          (cb'Image
                           & " 2 "
                           & Board_Spot'Image (sms.move)
                           & " "
                           & To_String (b)));
                  elsif sms.est_score = 0 then
                     board_list.Append
                       (To_Unbounded_String
                          (cb'Image
                           & " 0 "
                           & Board_Spot'Image (sms.move)
                           & " "
                           & To_String (b)));
                  else
                     raise Constraint_Error;
                  end if;
               end if;
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
      Per_Size_Maps : array (2 .. 72) of Move_Hash_Map.Map;
   begin
      for b_sms in Game_Book.Iterate loop
         declare
            b            : constant Game_State := Move_Hash_Map.Key (b_sms);
            board_pieces : constant Integer :=
              (72 - Integer (b.store (1)) - Integer (b.store (2)));
            sms          : constant Spot_Move_Score :=
              Move_Hash_Map.Element (b_sms);
         begin
            Per_Size_Maps (board_pieces).Insert (b, sms);
         end;
      end loop;

      for pieces_on_board in 2 .. 72 loop
         for player1_store in 0 .. 36 loop
            if 72 - pieces_on_board - player1_store >= 0
              and then 72 - pieces_on_board - player1_store <= 36
            then
               Dump_Move_Book_Local
                 (f,
                  Per_Size_Maps (pieces_on_board),
                  Piece_Count (pieces_on_board),
                  Piece_Count (player1_store));
            end if;
         end loop;
      end loop;
   end Dump_Move_Book;

   procedure Add_Missing (depth : Natural) is
   use Move_Hash_Set;
      count      : Ada.Containers.Count_Type := 0;
      iterations : Integer := 0;
      b          : Game_State;
      still_missing : Move_Hash_Set.Set := Empty_Set;
   begin
      while Unknown_Move_Book.Current_Use > 0 loop
         if count = 0 then
            iterations := @ + 1;
            count := Unknown_Move_Book.Current_Use;
            still_missing.Clear;
            Ada.Text_IO.Put_Line
              ("** Iteration "
               & iterations'Image
               & " Missing boards: "
               & count'Image);
         end if;
         if count mod 1_000_000 = 0 then
            Ada.Text_IO.Put_Line
              ("*** Iteration "
               & iterations'Image
               & " Boards left: "
               & count'Image);
         end if;            
         Unknown_Move_Book.Dequeue (b);
         -- Don't repeatedly do the same item at the same iterations;
         -- make sure we redo items at a higher iterations
         if not still_missing.Contains(b) then
            Add_Move (b, depth * iterations);
         end if;
         if not Is_Book_Move (b) and then not still_missing.Contains (b) then
            still_missing.Insert(b);   
         end if;
         count := @ - 1;
      end loop;
   end Add_Missing;

end Move_Book;
