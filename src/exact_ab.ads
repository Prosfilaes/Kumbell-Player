with Board;  use Board;
with Player; use Player;

package Exact_AB is
   type Move_Score_Type is record
      Move  : Move_Type;
      Score : Winner_Type;
      Exact : Boolean;
   end record;

   function Best_Move
     (b : Game_State_Type; depth : Natural) return Move_Score_Type
   with Pre => not Game_Over (b) and then b.curr_player = 1;
   Stack_Overflow_Error : exception;

end Exact_AB;
