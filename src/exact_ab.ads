with Board;  use Board;
with Player; use Player;
with Move_Book;

package Exact_AB is
   type Move_Score_Type is record
      Move  : Move_Type;
      Score : Winner_Type;
      Exact : Boolean;
   end record;

   function Best_Move
     (b : Game_State_Type; depth : Natural) return Move_Score_Type
   with Pre => not Game_Over (b) and then b.curr_player = 1;

   function Player_Search 
   (b : Game_State_Type; depth : Integer) return Move_Book.Option_Winner_Type;

end Exact_AB;
