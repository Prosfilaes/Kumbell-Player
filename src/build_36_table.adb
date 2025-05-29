pragma Restrictions (No_Obsolescent_Features);

with Board;       use Board;
with Ada.Text_IO; use Ada.Text_IO;
with Move_Book;

procedure Build_36_Table is

   procedure Print_Chunk
     (player1_score : Piece_Count; player2_score : Piece_Count)
   with
     Pre =>
       player1_score mod 2 = 0
       and then player2_score mod 2 = 0
       and then player1_score <= 36
       and then player2_score <= 36;

   procedure Print_Chunk_Rec
     (b : Board.Game_State; depth : Integer; min_spot : Board_Spot)
   is
      new_b : Board.Game_State;
   begin
      if depth = 0 then
         pragma Assert (Is_Legal_Board (b));
         if not Game_Over (b) then
            if not Move_Book.Is_Book_Move (b) then
               Move_Book.Add_Move (b, 5);
            end if;
         end if;
      else
         new_b := b;
         for i in Board_Spot'(min_spot) .. 12 loop
            new_b.board (i) := @ + 1;
            Print_Chunk_Rec (new_b, depth - 1, i);
            new_b.board (i) := @ - 1;
         end loop;
      end if;
   end Print_Chunk_Rec;

   procedure Print_Chunk
     (player1_score : Piece_Count; player2_score : Piece_Count)
   is
      depth : constant Integer :=
        72 - (Integer (player1_score) + Integer (player2_score));
      b     : Board.Game_State;
   begin
      Ada.Text_IO.Put_Line
        ("** Working on  "
         & depth'Image
         & " pieces on the board and "
         & player1_score'Image
         & " vs "
         & player2_score'Image);
      b.curr_player := 1;
      b.store (1) := player1_score;
      b.store (2) := player2_score;
      for i in Board_Spot'(1) .. 12 loop
         b.board (i) := 0;
      end loop;
      Print_Chunk_Rec (b, depth, 1);
      Move_Book.Add_Missing (5);
   end Print_Chunk;

begin
   Move_Book.Load_Book ("move_book36.table");

   for i in reverse Piece_Count'(18) .. 34 loop
       if i mod 2 = 0 then 
           Print_Chunk (36, i);
       end if;
   end loop; 

   Move_Book.Dump_Move_Book (Standard_Output);
end Build_36_Table;
