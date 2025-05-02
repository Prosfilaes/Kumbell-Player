with Board; use Board;

package Alpha_Beta is

    type Score is range -127 .. 127;
    type Spot_Move_Score is record
        move : Board_Spot;
        est_score : Score;
    end record;

    function Best_Move (b: Game_State; depth : Integer) return Spot_Move_Score;
    function Best_Move_Negamax (b: Game_State; depth : Integer) return Spot_Move_Score;
end Alpha_Beta;
