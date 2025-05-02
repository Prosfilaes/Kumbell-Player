with Ada.Text_IO; use Ada.Text_IO;
with Board;       use Board;
with Alpha_Beta; use Alpha_Beta;

procedure Two_Player is
   b : Game_State := Initialize;
   ab_move : Spot_Move_Score;
   num_moves : Integer := 0;
begin
   while num_moves < 100 and then not Game_Over (b) loop
      num_moves := num_moves + 1;
      b := First_Move (b);
      Put_Line ("Move " & num_moves'Image);
      Put_Line (To_String (b));
      if Game_Over (b) then
         exit;
      end if;
      ab_move := Alpha_Beta.Best_Move (b, 7);
      b := Move (b, ab_move.move);
      Put_Line (To_String (b));
   end loop;
end Two_Player;
