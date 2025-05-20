with Player; use Player;
with Move_Book;
with Ada.Text_IO;

package body Exact_AB is

   function Evaluate (b : Game_State) return Score
   with pre => Board.Game_Over (b);

   function Evaluate (b : Game_State) return Score is
      ret_score : Score;
      winner    : Winner_Type;
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
   end Evaluate;

   function Is_Solvable (b : Game_State) return Boolean is
      p1, p2 : Piece_Count;
   begin
      p1 := Player1_Board_Pieces (b);
      p2 := Player2_Board_Pieces (b);
      if b.curr_player = 2 then
         if p1 = 0 and b.board (7) /= p2 then
            return true;
         elsif b.board (1) = p1 and p1 <= 6 and b.board (7) /= p2 then
            return true;
         end if;
      else
         if p2 = 0 and b.board (1) /= p1 then
            return true;
         elsif b.board (7) = p2 and p2 <= 6 and b.board (1) /= p1 then
            return true;
         end if;
      end if;
      return False;
   end Is_Solvable;

   function Solve_Simple (b : Game_State) return Spot_Move_Score is
      p1, p2    : Piece_Count;
      new_store : Piece_Count;
   begin
      p1 := Player1_Board_Pieces (b);
      p2 := Player2_Board_Pieces (b);

      if b.curr_player = 2 then
         if (p1 = 0 and then b.board (7) /= p2)
           or else (b.board (1) = p1
                    and then p1 <= 6
                    and then b.board (7) /= p2)
         then
            for m in Board_Spot'(8) .. 12 loop
               if b.board (m) /= 0 then
                  -- m is effectively a null move; p1 is still 0.
                  -- Thus player 1 goes and loses because they have
                  -- no move. p2 gets added to Player 2's score.
                  new_store := b.store (2) + p2 + p1;
                  if b.store (1) > new_store then
                     return Spot_Move_Score'(m, -127, True);
                  elsif b.store (1) = new_store then
                     return Spot_Move_Score'(m, 0, True);
                  else
                     return Spot_Move_Score'(m, 127, True);
                  end if;
               end if;
            end loop;
         end if;
      else
         if (p2 = 0 and then b.board (1) /= p1)
           or else (b.board (7) = p2
                    and then p2 <= 6
                    and then b.board (1) /= p1)
         then
            for m in Board_Spot'(2) .. 6 loop
               if b.board (m) /= 0 then
                  -- m is effectively a null move; p2 is still 0.
                  -- Thus player 2 goes and loses because they have
                  -- no move. p1 gets added to Player 1's score.
                  new_store := b.store (1) + p1 + p2;
                  if b.store (2) > new_store then
                     return Spot_Move_Score'(m, -127, True);
                  elsif b.store (2) = new_store then
                     return Spot_Move_Score'(m, 0, True);
                  else
                     return Spot_Move_Score'(m, 127, True);
                  end if;
               end if;
            end loop;
         -- We check in Is_Simple so we don't have to call AB Search again.

         end if;
      end if;

      raise Constraint_Error with "No legal moves in degenerate position";
   end Solve_Simple;

   type Winner_Score is record
      resolved : Boolean;
      score    : Winner_Type;
   end record;

   function Player2_Search
     (b : Game_State; depth : Integer) return Winner_Score;

   function Player1_Search
     (b : Game_State; depth : Integer) return Winner_Score
   is
      end_array : Integer := 0;
      scores    : array (1 .. 6) of Winner_Score :=
        [others => Winner_Score'(False, 0)];
   begin
      if Game_Over (b) then
         return Winner_Score'(True, Board.Winner (b));
      end if;
      if Move_Book.Is_Book_Move (b) then
         declare
            sms : constant Spot_Move_Score := Move_Book.Get_Move (b);
         begin
            if sms.est_score = 127 then
               return Winner_Score'(True, -1);
            elsif sms.est_score = 0 then
               return Winner_Score'(True, 0);
            elsif sms.est_score = -127 then
               return Winner_Score'(True, 1);
            else
               raise Constraint_Error with "Invalid return from Move_Book";
            end if;
         end;
      end if;
      if depth = 0 then
         return Winner_Score'(False, 0);
      end if;
      for m in Board_Spot'(1) .. 6 loop
         if Is_Legal_Move (b, m) then
            end_array := @ + 1;
            scores (end_array) := Player2_Search (Move (b, m), depth - 1);
            if scores (end_array) = Winner_Score'(True, -1)
              or (b.store (2) = 36
                  and then scores (end_array) = Winner_Score'(True, 0))
            then
               return scores (end_array);
            end if;
         end if;
      end loop;
      declare
         best_move : Winner_Type := 1; -- -1 is player 1 win, 1 is player 2 win
      begin
         for i in 1 .. end_array loop
            if not scores (i).resolved then
               return Winner_Score'(False, 0);
            else
               if scores (i).score < best_move then
                  best_move := scores (i).score;
               end if;
            end if;
         end loop;
         return Winner_Score'(True, best_move);
      end;
   end Player1_Search;

   function Player2_Search
     (b : Game_State; depth : Integer) return Winner_Score
   is
      end_array : Integer := 0;
      scores    : array (1 .. 6) of Winner_Score :=
        [others => Winner_Score'(False, 0)];
   begin
      if Game_Over (b) then
         return Winner_Score'(True, Board.Winner (b));
      end if;
      if depth = 0 then
         return Winner_Score'(False, 0);
      end if;
      for m in Board_Spot'(7) .. 12 loop
         if Is_Legal_Move (b, m) then
            end_array := @ + 1;
            scores (end_array) := Player1_Search (Move (b, m), depth - 1);
            if scores (end_array) = Winner_Score'(True, 1)
              or (b.store (1) = 36
                  and then scores (end_array) = Winner_Score'(True, 0))
            then
               return scores (end_array);
            end if;
         end if;
      end loop;
      declare
         best_move : Winner_Type := -1;
      begin
         for i in 1 .. end_array loop
            if not scores (i).resolved then
               return Winner_Score'(False, 0);
            else
               if scores (i).score > best_move then
                  best_move := scores (i).score;
               end if;
            end if;
         end loop;
         return Winner_Score'(True, best_move);
      end;
   end Player2_Search;

   type Solved_Score is record
      resolved : Boolean;
      s        : Score;
   end record;

   function Alpha_Beta_Search
     (board : Game_State; alpha : Score; beta : Score; depth : Integer)
      return Solved_Score
   is
      best_score   : Score;
      this_score   : Solved_Score;
      new_alpha    : Score := alpha;
      cb           : Compressed_Board;
      all_resolved : Boolean := True;
   begin
      if Game_Over (board) then
         return Solved_Score'(True, Evaluate (board));
      end if;
      if Is_Solvable (board) then
         return Solved_Score'(True, Solve_Simple (board).est_score);
      end if;
      if depth = 0 then
         return Solved_Score'(False, 0);
      end if;
      if board.store (board.curr_player) = 36 then
         best_score := 0;
         if new_alpha < 0 then
            new_alpha := 0;
         end if;
      else
         best_score := -127;
      end if;
      for m in Board_Spot'(1) .. 12 loop
         if is_legal_move (board, m) then
            declare
               new_board : constant Game_State := Move (board, m);
            begin
               if new_board.curr_player = 1 then
                  cb := Compress (new_board);
               end if;
               if new_board.curr_player = 1
                 and then Move_Book.Is_Book_Move (new_board)
               then
                  this_score := Solved_Score'(True, -Move_Book.Get_Score (new_board));
               elsif new_board.curr_player = 1
                 and then Move_Book.Is_Missing_Move (new_board)
               then
                  Ada.Text_IO.Put_Line
                    (" **** derived from missing " & Compress_Base64 (cb));
                  this_score := Solved_Score'(False, 0);
               else
                  this_score :=
                    Alpha_Beta_Search
                      (new_board, -beta, -new_alpha, depth - 1);
                  this_score.s := -this_score.s;
               end if;
               if this_score.s >= beta and then this_score.resolved then
                  return this_score;
               end if;
               all_resolved := all_resolved and this_score.resolved;
               if this_score.s >= best_score then
                  best_score := this_score.s;
               end if;
               new_alpha := Score'Max (new_alpha, this_score.s);
            end;
         end if;
      end loop;
      if not all_resolved then
         return Solved_Score'(False, best_score);
      else
         return Solved_Score'(True, best_score);
      end if;
   end;

   function Best_Move (b : Game_State) return Spot_Move_Score is
      best_score   : Score := -127;
      best_move    : Board_Spot := 7;
      new_board    : Game_State;
      new_score    : Winner_Score;
      all_resolved : Boolean := True;
   begin
      if Is_Solvable (b) then
         return Solve_Simple (b);
      end if;
      -- If we have a winning solution, choose that, no matter
      -- whether or not we have other unsolved positions.
      -- Draws or loses need solutions at all points.
      if b.curr_player = 1 then
         for m in Board_Spot'(1) .. 6 loop
            if Is_Legal_Move (b, m) then
               if best_move = 7 then
                  best_move := m;
               end if;
               new_board := move (b, m);
               new_score := Player2_Search (new_board, 30);
               if new_score.score = -1 and new_score.resolved then
                  return Spot_Move_Score'(m, 127, True);
               end if;
               if b.store (2) = 36
                 and new_score.resolved
                 and new_score.score = 0
               then
                  return Spot_Move_Score'(m, 0, True);
               end if;
               if new_score.resolved and all_resolved then
                  if new_score.score = 0 then
                     best_score := 0;
                     best_move := m;
                  end if;
               else
                  all_resolved := false;
               end if;
            end if;
         end loop;
         return Spot_Move_Score'(best_move, best_score, all_resolved);
      else
         best_move := 1;
         for m in Board_Spot'(7) .. 12 loop
            if Is_Legal_Move (b, m) then
               if best_move = 1 then
                  best_move := m;
               end if;
               new_board := move (b, m);
               new_score := Player1_Search (new_board, 30);
               if new_score.score = 1 and new_score.resolved then
                  return Spot_Move_Score'(m, 127, True);
               end if;
               if b.store (1) = 36
                 and new_score.resolved
                 and new_score.score = 0
               then
                  return Spot_Move_Score'(m, 0, True);
               end if;
               if new_score.resolved and all_resolved then
                  if new_score.score = 0 then
                     best_score := 0;
                     best_move := m;
                  end if;
               else
                  all_resolved := false;
               end if;
            end if;
         end loop;
         return Spot_Move_Score'(best_move, best_score, all_resolved);
      end if;
   end Best_Move;

end Exact_AB;
