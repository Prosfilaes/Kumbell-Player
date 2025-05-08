with Board; use Board;

package Alpha_Beta is

    function Best_Move (b: Game_State; depth : Integer) return Spot_Move_Score;
    function Best_Move_Negamax (b: Game_State; depth : Integer) return Spot_Move_Score;
end Alpha_Beta;
