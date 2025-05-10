pragma Restrictions (No_Obsolescent_Features);

with Board;       use Board;
with Alpha_Beta;
with Ada.Text_IO; use Ada.Text_IO;
with Move_Book;

procedure Build_Board_Table is

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
      new_b : Board.Game_State := b;
      sms   : Spot_Move_Score;
      cb : Compressed_Board;
   begin
      if depth = 0 then
         pragma Assert (Is_Legal_Board (b));
         if not Game_Over (b) then
            cb := Compress (b);
            if Move_Book.Is_Book_Move (cb) then
               sms := Move_Book.Get_Move (cb, b.curr_player);
            else
               sms := Alpha_Beta.Best_Move (b, 22);
            end if;
            if sms.est_score = 127 then
               Put_Line
                 (Compress_Base64 (Compress (b))
                  & " 1 "
                  & Board_Spot'Image (sms.move));
            elsif sms.est_score = -127 then
               Put_Line
                 (Compress_Base64 (Compress (b))
                  & " 2 "
                  & Board_Spot'Image (sms.move));
            elsif sms.est_score = 0 then
               Put_Line
                 (Compress_Base64 (Compress (b))
                  & " 0 "
                  & Board_Spot'Image (sms.move));
            else
               Put_Line
                 ("*** Didn't conclude -"
                  & Compress_Base64 (Compress (b))
                  & " "
                  & sms.est_score'Image
                  & " "
                  & Board_Spot'Image (sms.move));
            end if;
         end if;
      else
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
      Put_Line ("");
      Put_Line
        ("== "
         & depth'Image
         & " pieces on the board and "
         & player1_score'Image
         & " vs "
         & player2_score'Image
         & " ==");
      b.curr_player := 1;
      b.store (1) := player1_score;
      b.store (2) := player2_score;
      for i in Board_Spot'(1) .. 12 loop
         b.board (i) := 0;
      end loop;
      Print_Chunk_Rec (b, depth, 1);
   end Print_Chunk;
   -- 0  1  1  0  0  0
   --<---
   --0  1  1  0  0  0
   -- Player 1:  36 Player 2:  32 Player to move:  2
   b : Board.Game_State;
begin
   if not Move_Book.Load_Book ("move_book.table") then
      Put_Line ("Failed to load move book");
   end if;
   if false then
      b.curr_player := 2;
      b.store (1) := 36;
      b.store (2) := 32;
      for i in Board_Spot'(1) .. 12 loop
         b.board (i) := 0;
      end loop;
      b.board (2) := 1;
      b.board (3) := 1;
      b.board (10) := 1;
      b.board (11) := 1;
      Ada.Text_IO.Put_Line (To_String (b));
      Ada.Text_IO.Put_Line (Alpha_Beta.Best_Move (b, 16)'Image);
      Ada.Text_IO.Put_Line (Move_Book.Get_Move (Compress (b), 2)'Image);
      return;
   end if;

   Print_Chunk (36, 34);
   Print_Chunk (34, 36);

   Print_Chunk (36, 32);
   Print_Chunk (34, 34);
   Print_Chunk (32, 36);

   Print_Chunk (36, 30);
   Print_Chunk (34, 32);
   Print_Chunk (32, 34);
   Print_Chunk (30, 36);

   Print_Chunk (36, 28);
   Print_Chunk (34, 30);
   Print_Chunk (32, 32);
   Print_Chunk (30, 34);
   Print_Chunk (28, 36);

   for i in Piece_Count'(26) .. 36 loop
      if i mod 2 = 0 then
         Print_Chunk (i, 62 - i);
      end if;
   end loop;

   for i in Piece_Count'(24) .. 36 loop
      if i mod 2 = 0 then
         Print_Chunk (i, 60 - i);
      end if;
   end loop;

end Build_Board_Table;
