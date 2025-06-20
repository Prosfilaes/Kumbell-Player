with Move_Book; use Move_Book;

package body Exact_AB is

   function Player1_Search
     (b     : Game_State_Type;
      alpha : Winner_Type;
      beta  : Winner_Type;
      depth : Integer) return Option_Winner_Type;

   function Player2_Search
     (b     : Game_State_Type;
      alpha : Winner_Type;
      beta  : Winner_Type;
      depth : Integer) return Option_Winner_Type;

   function Player_Search
     (b     : Game_State_Type;
      alpha : Winner_Type;
      beta  : Winner_Type;
      depth : Integer) return Option_Winner_Type
   with inline
   is
   begin
      if b.curr_player = 1 then
         return Player1_Search (b, alpha, beta, depth);
      else
         return Player2_Search (b, alpha, beta, depth);
      end if;
   end Player_Search;

   function Player1_Search
     (b     : Game_State_Type;
      alpha : Winner_Type;
      beta  : Winner_Type;
      depth : Integer) return Option_Winner_Type
   is
      end_array    : Integer := 0;
      scores       : array (1 .. 6) of Option_Winner_Type :=
        [others => Option_Winner_Type'(False, 0)];
      new_beta     : Winner_Type := beta;
      all_resolved : Boolean := True;
      value        : Winner_Type := 1;
      cb           : constant Compressed_Board := Compress (b);
   begin
      if Game_Over (b) then
         return Option_Winner_Type'(True, Board.Winner (b));
      end if;

      declare
         ow : constant Option_Winner_Type := Move_Book.Get_Score (cb);
      begin
         if ow.Found then
            return ow;
         end if;
      end;

      if depth <= 0 then
         Move_Book.Missing_Move_Insert (cb);
         return Option_Winner_Type'(False, 0);
      end if;

      for m of Every_Move (b) loop
         end_array := @ + 1;
         scores (end_array) :=
           Player_Search (Move (b, m), alpha, new_beta, depth);
         if (scores (end_array).Found and scores (end_array).Winner <= alpha)
         then
            return scores (end_array);
         end if;
         if scores (end_array).Found and scores (end_array).Winner < new_beta
         then
            new_beta := scores (end_array).Winner;
         end if;
         value := Winner_Type'Min (value, scores (end_array).Winner);
         all_resolved := @ and scores (end_array).Found;
      end loop;

      if all_resolved then
         return Option_Winner_Type'(True, value);
      else
         return Option_Winner_Type'(False, 0);
      end if;
   end Player1_Search;

   function Player2_Search
     (b     : Game_State_Type;
      alpha : Winner_Type;
      beta  : Winner_Type;
      depth : Integer) return Option_Winner_Type
   is
      end_array    : Integer := 0;
      scores       : array (1 .. 6) of Option_Winner_Type :=
        [others => Option_Winner_Type'(False, 0)];
      new_alpha    : Winner_Type := alpha;
      all_resolved : Boolean := True;
      value        : Winner_Type := -1;
   begin
      if Game_Over (b) then
         return Option_Winner_Type'(True, Board.Winner (b));
      end if;
      for m of Every_Move (b) loop
         if Is_Legal_Move (b, m) then
            end_array := @ + 1;
            scores (end_array) :=
              Player_Search (Move (b, m), new_alpha, beta, depth - 1);
            if -- "scores (end_array) = Option_Winner'(True, 1) or" subsumed by beta
               (scores (end_array).Found and scores (end_array).Winner >= beta)
            then
               return scores (end_array);
            end if;
            if scores (end_array).Found
              and scores (end_array).Winner > new_alpha
            then
               new_alpha := scores (end_array).Winner;
            end if;
            value := Winner_Type'Max (value, scores (end_array).Winner);
            all_resolved := @ and scores (end_array).Found;
         end if;
      end loop;

      if all_resolved then
         return Option_Winner_Type'(True, value);
      else
         return Option_Winner_Type'(False, 0);
      end if;
   end Player2_Search;

   function Best_Move
     (b : Game_State_Type; depth : Natural) return Move_Score_Type
   is
      best_score   : Winner_Type := 1;
      best_move    : Move_Type;
      new_board    : Game_State_Type;
      new_score    : Option_Winner_Type;
      all_resolved : Boolean := True;
      found_move   : Boolean := False;
   begin
      -- If we have a winning solution, choose that, no matter
      -- whether or not we have other unsolved positions.
      -- Draws or loses need solutions at all points, unless
      -- the best we can pull is a draw
      for m of Every_Move (b) loop

         if not found_move then
            best_move := m;
            found_move := True;
         end if;
         new_board := move (b, m);
         new_score := Player2_Search (new_board, -1, 1, depth);
         if new_score.Winner = -1 and new_score.Found then
            return Move_Score_Type'(m, -1, True);
         end if;
         if new_score.Found and all_resolved then
            if new_score.Winner < best_score then
               best_score := new_score.Winner;
               best_move := m;
            end if;
         else
            all_resolved := false;
         end if;

      end loop;
      return Move_Score_Type'(best_move, best_score, all_resolved);
   end Best_Move;

end Exact_AB;
