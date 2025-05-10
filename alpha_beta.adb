with Player;    use Player;
with Move_Book;

package body Alpha_Beta is

   type Score_with_Exact is record
      est_score : Score;
      exact     : Boolean := False;
   end record;

   function Evaluate (board : Game_State) return Score_with_Exact is
      ret_score : Score_with_Exact;
   begin
      if board.store (board.curr_player) >= 37 then
         return Score_with_Exact'(127, True);
      elsif board.store (Next (board.curr_player)) >= 37 then
         return Score_with_Exact'(-127, True);
      end if;
      ret_score.est_score :=
        Score (board.store (board.curr_player))
        - Score (board.store (Next (board.curr_player)));
      return ret_score;
   end evaluate;

   function Alpha_Beta_Search
     (board : Game_State; depth : Integer; alpha : Score; beta : Score)
      return Score_with_Exact
   is
      best_score : Score_with_Exact;
      new_board  : Game_State;
      this_score : Score_with_Exact;
      new_alpha  : Score := alpha;
      cb         : Compressed_Board;
   begin
      if depth = 0 or else Game_Over (board) then
         return Evaluate (board);
      end if;
      best_score.est_score := -127;
      for m in Board_Spot'(1) .. 12 loop
         if is_legal_move (board, m) then
            new_board := Move (board, m);
            cb := Compress (new_board);
            if new_board.curr_player = 2
              or else not Move_Book.Is_Book_Move (cb)
            then
               this_score :=
                 Alpha_Beta_Search (new_board, depth - 1, -beta, -new_alpha);
               this_score.est_score := -this_score.est_score;
            else
               this_score.est_score := -Move_Book.Get_Score (cb);
               this_score.exact := True;
            end if;
            if this_score.est_score >= beta then
               return this_score;
            end if;
            if this_score.est_score > best_score.est_score
              or else (this_score.est_score = best_score.est_score
                       and then this_score.exact)
            then
               best_score := this_score;
            end if;
            new_alpha := Score'Max (new_alpha, this_score.est_score);
            if new_alpha >= beta then
               return best_score;
            end if;
         end if;
      end loop;
      return best_score;
   end;

   function Best_Move (b : Game_State; depth : Integer) return Spot_Move_Score
   is
      best_score : Score_with_Exact := Score_with_Exact'(-127, False);
      best_move  : Board_Spot := 1;
      new_board  : Game_State;
      new_score  : Score_with_Exact;
   begin
      for m in Board_Spot'Range loop
         if Is_Legal_Move (b, m) then
            new_board := move (b, m);
            new_score := alpha_beta_search (new_board, depth - 1, -127, 127);
            new_score.est_score := -new_score.est_score;
            if new_score.est_score > best_score.est_score or else
               (new_score.est_score = best_score.est_score
                and then new_score.exact)
            then
               best_score := new_score;
               best_move := m;
            end if;
         end if;
      end loop;
      return Spot_Move_Score'(best_move, best_score.est_score, best_score.exact);
   end Best_Move;

end Alpha_Beta;
