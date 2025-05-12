with Ada.Text_IO; use Ada.Text_IO;
with Board;       use Board;
with Alpha_Beta;  use Alpha_Beta;

procedure Two_Player is
   b         : Game_State := Initialize;
   ab_move   : Spot_Move_Score;
   num_moves : Integer := 0;
begin
   while num_moves < 100 loop
      num_moves := num_moves + 1;
      if False then
         b := First_Move (b);
      end if;
      Put_Line ("Move " & num_moves'Image);
      if Game_Over (b) then
         Put_Line ("Winner: " & Winner (b)'Image);
         exit;
      end if;
      ab_move := Alpha_Beta.Best_Move (b, 20);
      if ab_move.exact then
         Put_Line ("Best move: " & Board_Spot'Image (ab_move.move));
         Put_Line ("Score: " & ab_move.est_score'Image);
      else
         Put_Line ("No best move found");
      end if;
      b := Move (b, ab_move.move);
      Put_Line (To_String (b));
   end loop;
end Two_Player;
