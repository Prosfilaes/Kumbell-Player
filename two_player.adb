with Ada.Text_IO; use Ada.Text_IO;
with Board;       use Board;
with Alpha_Beta;

procedure Two_Player is
   b         : Game_State;
   ab_move   : Spot_Move_Score;
   num_moves : Integer;
begin
   for ab_depth in 1 .. 15 loop
      Put_Line ("Depth " & ab_depth'Image);
      Put_Line ("");
      b := Initialize;
      num_moves := 0;
      loop
         num_moves := num_moves + 1;
         Put_Line
           ("Move "
            & num_moves'Image
            & " Player "
            & b.curr_player'Image
            & " to move");
         ab_move := Alpha_Beta.Best_Move (b, ab_depth, true);
         if ab_move.exact then
            Put_Line ("Optimal move found");
         else
            Put_Line ("No optimal move found");
         end if;
         Put_Line ("Best move: " & Board_Spot'Image (ab_move.move));
         Put_Line
           ("Move Score (positive good for player "
            & b.curr_player'Image
            & "): "
            & ab_move.est_score'Image);
         b := Move (b, ab_move.move);
         Put_Line (To_String (b));
         if Game_Over (b) then
            Put_Line ("Game over!");
            case Winner (b) is
               when 0 =>
                  Put_Line ("Draw");

               when -1 =>
                  Put_Line ("Player 1 wins");

               when 1 =>
                  Put_Line ("Player 2 wins");

               when others =>
                  Put_Line ("Unknown winner");
                  raise Constraint_Error
                    with "Unknown winner" & Winner (b)'Image;
            end case;
            exit;
         end if;
      end loop;
   end loop;
end Two_Player;
