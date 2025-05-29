with Board; use Board;

package Alpha_Beta is

    function Best_Move (b: Game_State; depth : Integer; Emit_Move_Book : Boolean := False) return Spot_Move_Score;

end Alpha_Beta;
