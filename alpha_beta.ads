with Board; use Board;

package Alpha_Beta is

    type Score is range -127 .. 127;
    function Best_Move (b: Game_State; depth : Integer) return Score;
end Alpha_Beta;
