with Board; use Board;

package Exact_AB is

    function Best_Move (b: Game_State) return Spot_Move_Score
    with Pre => not Game_Over (b);
    Stack_Overflow_Error : exception;

end Exact_AB;
