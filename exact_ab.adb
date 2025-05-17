with Player; use Player;
with Move_Book;

package body Exact_AB is

   function Evaluate (b : Game_State) return Score
   with pre => Board.Game_Over (b);

   function Alpha_Beta_Search
     (board : Game_State; alpha : Score; beta : Score; depth : Integer)
      return Score;

   function Evaluate (b : Game_State) return Score is
      ret_score : Score;
      winner    : Integer;
   begin
      winner := Board.Winner (b);
      if winner = 0 then
         ret_score := 0;
      elsif (b.curr_player = 1 and then winner = -1)
        or else (b.curr_player = 2 and then winner = 1)
      then
         ret_score := 127;
      else
         -- (Winner = -1 and p = 2) or (winner = 1 and p = 1)
         ret_score := -127;
      end if;
      return ret_score;
   end evaluate;

   function Is_Degenerate (b : Game_State) return Boolean is
      p : Piece_Count;
   begin
      if b.curr_player = 2 then
         p :=
           b.board (1)
           + b.board (2)
           + b.board (3)
           + b.board (4)
           + b.board (5)
           + b.board (6);
         if p = 0 then
            return True;
         end if;
      else
         p :=
           b.board (7)
           + b.board (8)
           + b.board (9)
           + b.board (10)
           + b.board (11)
           + b.board (12);
         if p = 0 then
            return True;
         end if;
      end if;
      return False;
   end Is_Degenerate;

   function Solve_Degenerate (b : Game_State) return Spot_Move_Score is
      new_board  : Game_State;
      new_score  : Score;
      p : Piece_Count;
   begin
      if b.curr_player = 2 then
         p :=
           b.board (1)
           + b.board (2)
           + b.board (3)
           + b.board (4)
           + b.board (5)
           + b.board (6);
         if p = 0 then
            for m in Board_Spot'(8) .. 12 loop
               if b.board (m) /= 0 then
                  new_board := Move (b, m);
                  new_score := Alpha_Beta_Search (new_board, -127, 127, 3);
                  return Spot_Move_Score'(m, -new_score, True);
               end if;
            end loop;
            new_board := Move (b, 7);
            new_score := Alpha_Beta_Search (new_board, -127, 127, 3);
            return Spot_Move_Score'(7, -new_score, True);
         end if;
      else
         p :=
           b.board (7)
           + b.board (8)
           + b.board (9)
           + b.board (10)
           + b.board (11)
           + b.board (12);
         if p = 0 then
            for m in Board_Spot'(2) .. 6 loop
               if b.board (m) /= 0 then
                  new_board := Move (b, m);
                  new_score := Alpha_Beta_Search (new_board, -127, 127, 3);
                  return Spot_Move_Score'(m, -new_score, True);
               end if;
            end loop;
            new_board := Move (b, 1);
            new_score := Alpha_Beta_Search (new_board, -127, 127, 3);
            return Spot_Move_Score'(1, -new_score, True);
         end if;
      end if;

      raise Constraint_Error with
        "No legal moves in degenerate position";
   end Solve_Degenerate;

   function Alpha_Beta_Search
     (board : Game_State; alpha : Score; beta : Score; depth : Integer)
      return Score
   is
      best_score : Score;
      new_board  : Game_State;
      this_score : Score;
      new_alpha  : Score := alpha;
      cb         : Compressed_Board;
   begin
      if depth = 0 then
         raise Stack_Overflow_Error;
      end if;
      if Game_Over (board) then
         return Evaluate (board);
      end if;
      if Is_Degenerate (board) then
         return Solve_Degenerate (board).est_score;
      end if;
      if board.store(board.curr_player) = 36 then
         best_score := 0;
         if new_alpha < 0 then
            new_alpha := 0;
         end if;
      else
         best_score := -127;
      end if;

      for m in Board_Spot'(1) .. 12 loop
         if is_legal_move (board, m) then
            new_board := Move (board, m);
            if new_board.curr_player = 1 then
               cb := Compress (new_board);
            end if;
            if new_board.curr_player = 1 and then Move_Book.Is_Book_Move (cb)
            then
               this_score := Move_Book.Get_Score (cb);
            else
               this_score :=
                 Alpha_Beta_Search (new_board, -beta, -new_alpha, depth - 1);
               this_score := -this_score;
            end if;
            if this_score >= beta then
               return this_score;
            end if;
            if this_score > best_score then
               best_score := this_score;
            end if;
            new_alpha := Score'Max (new_alpha, this_score);
         end if;
      end loop;
      return best_score;
   end;

   function Best_Move (b : Game_State) return Spot_Move_Score is
      best_score : Score := -127;
      best_move  : Board_Spot := 1;
      new_board  : Game_State;
      new_score  : Score;
   begin
      if Is_Degenerate (b) then
         return Solve_Degenerate (b);
      end if;
      for m in Board_Spot'Range loop
         if Is_Legal_Move (b, m) then
            new_board := move (b, m);
            new_score := alpha_beta_search (new_board, -127, 127, 30);
            new_score := -@;
            if new_score > best_score then
               best_score := new_score;
               best_move := m;
            end if;
         end if;
      end loop;
      return Spot_Move_Score'(best_move, best_score, True);
   end Best_Move;

end Exact_AB;
