with Player; use Player;
with Move_Book;
with Ada.Text_IO;

package body Exact_AB is

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
     (b : Game_State; alpha : Winner_Type; beta : Winner_Type; depth : Integer)
      return Winner_Score;

   function Player1_Search
     (b : Game_State; alpha : Winner_Type; beta : Winner_Type; depth : Integer)
      return Winner_Score
   is
      end_array    : Integer := 0;
      scores       : array (1 .. 6) of Winner_Score :=
        [others => Winner_Score'(False, 0)];
      new_beta     : Winner_Type := beta;
      all_resolved : Boolean := True;
      value        : Winner_Type := 1;
   begin
      --Ada.Text_IO.Put_Line ("1 -- " & To_String(b) & " a:" & alpha'Image & " b:" & beta'Image & " depth:" & depth'Image);
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
      --Ada.Text_IO.Put_Line ("1 -- not GO or Book Move");
      if depth <= 0 then
         Move_Book.Missing_Move_Insert (b);
         return Winner_Score'(False, 0);
      end if;

      for m in Board_Spot'(1) .. 6 loop
         if Is_Legal_Move (b, m) then
            end_array := @ + 1;
            scores (end_array) :=
              Player2_Search (Move (b, m), alpha, new_beta, depth);
            if -- "scores (end_array) = Winner_Score'(True, -1) or" subsumed by alpha
               (b.store (2) = 36
                and then scores (end_array) = Winner_Score'(True, 0))
              or (scores (end_array).resolved
                  and scores (end_array).score <= alpha)
            then
               return scores (end_array);
            end if;
            if scores (end_array).resolved
              and scores (end_array).score < new_beta
            then
               new_beta := scores (end_array).score;
            end if;
            value := Winner_Type'Min (value, scores (end_array).score);
            all_resolved := @ and scores (end_array).resolved;
         end if;
      end loop;

      if all_resolved then
         return Winner_Score'(True, value);
      else
         return Winner_Score'(False, 0);
      end if;
   end Player1_Search;

   function Player2_Search
     (b : Game_State; alpha : Winner_Type; beta : Winner_Type; depth : Integer)
      return Winner_Score
   is
      end_array    : Integer := 0;
      scores       : array (1 .. 6) of Winner_Score :=
        [others => Winner_Score'(False, 0)];
      new_alpha    : Winner_Type := alpha;
      all_resolved : Boolean := True;
      value        : Winner_Type := -1;
   begin
      --Ada.Text_IO.Put_Line ("2 -- " & To_String(b) & " a:" & alpha'Image & " b:" & beta'Image & " depth:" & depth'Image);
      if Game_Over (b) then
         return Winner_Score'(True, Board.Winner (b));
      end if;
      --Ada.Text_IO.Put_Line ("2 -- not GO");
      for m in Board_Spot'(7) .. 12 loop
         if Is_Legal_Move (b, m) then
            end_array := @ + 1;
            scores (end_array) :=
              Player1_Search (Move (b, m), new_alpha, beta, depth - 1);
            if -- "scores (end_array) = Winner_Score'(True, 1) or" subsumed by beta
               (b.store (1) = 36
                and then scores (end_array) = Winner_Score'(True, 0))
              or (scores (end_array).resolved
                  and scores (end_array).score >= beta)
            then
               return scores (end_array);
            end if;
            if scores (end_array).resolved
              and scores (end_array).score > new_alpha
            then
               new_alpha := scores (end_array).score;
            end if;
            value := Winner_Type'Max (value, scores (end_array).score);
            all_resolved := @ and scores (end_array).resolved;
         end if;
      end loop;

      if all_resolved then
         return Winner_Score'(True, value);
      else
         return Winner_Score'(False, 0);
      end if;
   end Player2_Search;

   function Best_Move (b : Game_State; depth : Natural) return Spot_Move_Score
   is
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
      -- Draws or loses need solutions at all points, unless
      -- the best we can pull is a draw
      if b.curr_player = 1 then
         for m in Board_Spot'(1) .. 6 loop
            if Is_Legal_Move (b, m) then
               if best_move = 7 then
                  best_move := m;
               end if;
               new_board := move (b, m);
               new_score := Player2_Search (new_board, -1, 1, depth);
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
               new_score := Player1_Search (new_board, -1, 1, depth);
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
