with Board; use Board;
with Player;
package Alpha_Beta is

    type Score is range -127 .. 127;
    function Best_Move (b: Game_State; depth : Integer; p : Player.Player) return Score;
end Alpha_Beta;
