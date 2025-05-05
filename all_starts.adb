pragma Restrictions (No_Obsolescent_Features);

with Board; use Board;
with Ada.Text_IO;
with Random;
with Ada.Command_Line;
with Alpha_Beta;

procedure All_Starts is
   bstart    : Game_State := Initialize;
   num_games : constant Integer := 100000;

   function Play_Game (board : Game_State) return Integer is
      b : Game_State := board;
   begin
      loop
         if Game_Over (b) then
            return Winner (b);
         end if;
         b := Move (b, Random.Random_Move (b));
      end loop;
   end Play_Game;

begin
   for i in 1 .. Ada.Command_Line.Argument_Count loop
      declare
         arg : constant String := Ada.Command_Line.Argument (i);
      begin
         if Is_Legal_Move (bstart, Board_Spot'Value (arg)) then
            bstart := Move (bstart, Board_Spot'Value (arg));
            Ada.Text_IO.Put_Line ("Making move " & arg);
         else
            Ada.Text_IO.Put_Line ("Illegal move " & arg);
            return;
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Starting board:");
   Ada.Text_IO.Put_Line (To_String (bstart));
   Ada.Text_IO.Put_Line ("Game over: " & Game_Over (bstart)'Image);
   Ada.Text_IO.Put_Line ("A-B call (depth 16): " & Alpha_Beta.Best_Move (bstart, 16)'Image);
   for i in Board_Spot'(1) .. 12 loop
      declare
         player1_wins : Integer := 0;
         player2_wins : Integer := 0;
         draws        : Integer := 0;
         winner       : Integer;
         board1       : Game_State;
      begin
         if is_legal_move (bstart, i) then
            board1 := Move (bstart, i);

            for loop_count in 1 .. num_games loop
               winner := Play_Game (board1);
               if winner = -1 then
                  player1_wins := player1_wins + 1;
               elsif winner = 1 then
                  player2_wins := player2_wins + 1;
               else
                  draws := draws + 1;
               end if;
            end loop;

            Ada.Text_IO.Put_Line ("For game starting with " & i'Image);
            Ada.Text_IO.Put_Line (" player 1 wins " & player1_wins'Image);
            Ada.Text_IO.Put_Line (" player 2 wins " & player2_wins'Image);
            Ada.Text_IO.Put_Line (" draws " & draws'Image);
         end if;
      end;
   end loop;
end All_Starts;
