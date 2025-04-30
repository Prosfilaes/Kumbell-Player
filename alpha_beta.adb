with Ada.Text_IO;
with Player; use Player;

package body Alpha_Beta is

    function Evaluate (board : Game_State) return Score is
        ret_score : Score;
    begin
        if board.store(board.curr_player) >= 37 then
            return 127;
        elsif board.store(Next(board.curr_player)) >= 37 then
            return -127;
        end if;
        ret_score := Score(board.store(board.curr_player)) - Score(board.store (Next(board.curr_player)));
        Ada.Text_IO.Put_Line (To_String (board));
        Ada.Text_IO.Put_Line (ret_score'Image);
        return ret_score;
    end evaluate;

    function Alpha_Beta_Search (board : Game_State; depth : Integer; alpha : Score; beta : Score) return Score is
        best_score : Score;
        new_board : Game_State;
        this_score : Score;
        new_alpha : Score := alpha;
        new_beta : Score := beta;
    begin
        if depth = 0 or else Game_Over (board) then
            return Evaluate (board);
        end if;
        best_score := -127;
        for m in Board_Spot'(1) .. 12 loop
            if is_legal_move (board, m) then
                new_board := Move(board, m);
                this_score := -Alpha_Beta_Search (new_board, depth - 1, new_alpha, new_beta);
                --if this_score >= new_beta then
                --    return this_score;
                --end if;
                best_score := Score'Max (best_score, this_score);
                --new_alpha := Score'Max (new_alpha, this_score);
            end if;
        end loop;
        return best_score;
    end;

    function Best_Move (b: Game_State; depth : Integer) return Score is
        best_score : Score := -127;
        best_move : Board_Spot := 1;
        new_board : Game_State;
        new_score : Score;
    begin
        for m in Board_Spot'Range loop
            if Is_Legal_Move(b, m) then
            	new_board := move (b, m);
                new_score := - alpha_beta_search(new_board, depth - 1, -127, 127);
                Ada.Text_IO.Put ("Move ");
                Ada.Text_IO.Put (m'Image);
                Ada.Text_IO.Put (": "); 
                Ada.Text_IO.Put_Line (new_score'Image);
                if new_score > best_score then
            	    best_score := new_score;
                    best_move := m;
                end if;
            end if;
         end loop;
         return best_score;
    end Best_Move;

end Alpha_Beta;
