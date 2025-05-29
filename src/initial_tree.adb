with Ada.Text_IO;
with Board;       use Board;
with Move_Book; use Move_Book;
with Alpha_Beta;

procedure Initial_Tree is

   b         : constant Game_State := Initialize;

   procedure Player_2_Move (b : Game_State; depth : Integer);
   procedure Player_1_Move (b : Game_State; depth : Integer) is
      ab_move : Spot_Move_Score;
      cb : Compressed_Board;
   begin
      Ada.Text_IO.Put_Line (To_String (b));
      if depth <= 0 then
         cb := Compress (b);
         if Is_Book_Move (cb) then
            ab_move := Get_Move (cb, b.curr_player);
            Ada.Text_IO.Put_Line (ab_move.move'Image & " is optimal, giving score" & ab_move.est_score'Image);
         else
            ab_move := Alpha_Beta.Best_Move (b, 16, True);
            if ab_move.exact then
               Ada.Text_IO.Put_Line (ab_move.move'Image & " is optimal, giving score" & ab_move.est_score'Image);
            else
               Ada.Text_IO.Put_Line ("*** Didn't conclude - " & ab_move.est_score'Image);
            end if;
         end if;
         return;
      end if;
      for m in Board_Spot'(1) .. 6 loop
         Ada.Text_IO.Put_Line ("Player 1 Moves " & m'Image);
         Player_2_Move (Move (b, m), depth - 1);
      end loop;
      Ada.Text_IO.Put_Line ("--- End Player 1 Move ---");
   end Player_1_Move;

   procedure Player_2_Move (b : Game_State; depth : Integer) is
   begin
      Ada.Text_IO.Put_Line (To_String (b));
      for m in Board_Spot'(7) .. 12 loop
         Ada.Text_IO.Put_Line ("Player 2 Moves " & m'Image);
         Player_1_Move (Move (b, m), depth - 1);
      end loop;
      Ada.Text_IO.Put_Line ("--- End Player 2 Move ---");
   end Player_2_Move;
begin

   Ada.Text_IO.Put_Line (To_String (b));
   for m in Board_Spot'(1) .. 6 loop
      Ada.Text_IO.Put_Line ("Player 1 Moves " & m'Image);
      Player_2_Move (Move (b, m), 1);
   end loop;
end Initial_Tree;
