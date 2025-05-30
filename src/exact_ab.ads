with Board; use Board;
with Move_Book;

package Exact_AB is

    function Best_Move (b: Game_State_Type; depth : Natural) return Move_Book.Move_Score_Type
    with Pre => not Game_Over (b);
    Stack_Overflow_Error : exception;

end Exact_AB;
