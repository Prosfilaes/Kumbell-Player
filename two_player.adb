with Ada.Text_IO; use Ada.Text_IO;
with Board;       use Board;

procedure Two_Player is
   b : Game_State := Initialize;
   num_moves : Integer := 0;
begin
   while num_moves < 10 and then not Game_Over (b) loop
      Put_Line (To_String (b));
      b := First_Move (b);
      num_moves := num_moves + 1;
   end loop;
end Two_Player;
