with Ada.Text_IO;
with Player; use Player;
with Move_Book;

package body Alpha_Beta is

   Move_Book_On : Boolean := False;
   New_Book     : Ada.Text_IO.File_Type;

   type Score_with_Exact is record
      est_score : Score;
      exact     : Boolean := False;
   end record;

   function Evaluate (b : Game_State) return Score_with_Exact is
      ret_score : Score_with_Exact;
      winner    : Integer;
   begin
      if Game_Over (b) then
         winner := Board.Winner (b);
         if winner = 0 then
            ret_score.est_score := 0;
         elsif (b.curr_player = 1 and then winner = -1)
           or else (b.curr_player = 2 and then winner = 1)
         then
            ret_score.est_score := 127;
         else
            -- (Winner = -1 and p = 2) or (winner = 1 and p = 1)
            ret_score.est_score := -127;
         end if;
         ret_score.exact := True;
         return ret_score;
      end if;
      ret_score.est_score :=
        Score (b.store (b.curr_player))
        - Score (b.store (Next (b.curr_player)));
      return ret_score;
   end evaluate;

   function Alpha_Beta_Search
     (board        : Game_State;
      depth        : Integer;
      alpha        : Score;
      beta         : Score;
      Move_Book_On : Boolean := False) return Score_with_Exact
   is
      best_score : Score_with_Exact;
      new_board  : Game_State;
      this_score : Score_with_Exact;
      new_alpha  : Score := alpha;
      cb         : Compressed_Board;
      p          : Piece_Count;
   begin
      if depth = 0 or else Game_Over (board) then
         return Evaluate (board);
      end if;
      if board.curr_player = 2 then
         p :=
           board.board (1)
           + board.board (2)
           + board.board (3)
           + board.board (4)
           + board.board (5)
           + board.board (6);
         if p = 0 then
            for m in Board_Spot'(8) .. 12 loop
               if board.board (m) /= 0 then
                  new_board := Move (board, m);
                  this_score :=
                    Alpha_Beta_Search
                      (new_board, depth - 1, -beta, -new_alpha);
                  this_score.est_score := -this_score.est_score;
                  return this_score;
               end if;
            end loop;
         end if;
      else
         p :=
           board.board (7)
           + board.board (8)
           + board.board (9)
           + board.board (10)
           + board.board (11)
           + board.board (12);
         if p = 0 then
            for m in Board_Spot'(2) .. 6 loop
               if board.board (m) /= 0 then
                  new_board := Move (board, m);
                  this_score :=
                    Alpha_Beta_Search
                      (new_board, depth - 1, -beta, -new_alpha);
                  this_score.est_score := -this_score.est_score;
                  return this_score;
               end if;
            end loop;
         end if;
      end if;
      best_score.est_score := -127;

      for m in Board_Spot'(1) .. 12 loop
         if is_legal_move (board, m) then
            new_board := Move (board, m);
            if new_board.curr_player = 1 then
               cb := Compress (new_board);
            end if;
            if new_board.curr_player = 1 and then Move_Book.Is_Book_Move (cb)
            then
               this_score.est_score := -Move_Book.Get_Score (cb);
               this_score.exact := True;
            else
               this_score :=
                 Alpha_Beta_Search (new_board, depth - 1, -beta, -new_alpha);
               this_score.est_score := -this_score.est_score;
            end if;

            if this_score.est_score >= beta then
                           if Move_Book_On
                 and then board.curr_player = 1
                 and then Is_Compressable (board)
               then
                  Move_Book.Add_Move (New_Book, board, depth);
               end if;
               return this_score;
            end if;
            if this_score.est_score > best_score.est_score
              or else (this_score.est_score = best_score.est_score
                       and then this_score.exact)
            then
               best_score := this_score;
            end if;
            new_alpha := Score'Max (new_alpha, this_score.est_score);
         end if;
      end loop;
      if Move_Book_On and then board.curr_player = 1 then
         Move_Book.Add_Move (New_Book, board, depth);
      end if;
      return best_score;
   end;

   function Best_Move
     (b : Game_State; depth : Integer; Emit_Move_Book : Boolean := False)
      return Spot_Move_Score
   is
      best_score : Score_with_Exact := Score_with_Exact'(-127, False);
      best_move  : Board_Spot := 1;
      new_board  : Game_State;
      new_score  : Score_with_Exact;
   begin
      if Emit_Move_Book then
         if not Move_Book_On then
            Move_Book_On := True;
            Ada.Text_IO.Create
              (New_Book, Ada.Text_IO.Append_File, "aux_move_book.table");
         end if;
      end if;
      for m in Board_Spot'Range loop
         if Is_Legal_Move (b, m) then
            new_board := move (b, m);
            new_score :=
              alpha_beta_search
                (new_board, depth - 1, -127, 127, Emit_Move_Book);
            new_score.est_score := -new_score.est_score;
            if new_score.est_score > best_score.est_score
              or else (new_score.est_score = best_score.est_score
                       and then new_score.exact)
            then
               best_score := new_score;
               best_move := m;
            end if;
         end if;
      end loop;
      return
        Spot_Move_Score'(best_move, best_score.est_score, best_score.exact);
   end Best_Move;

end Alpha_Beta;
