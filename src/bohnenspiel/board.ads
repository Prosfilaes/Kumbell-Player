with Player; use Player;

package Board is

   type Board_Type is private;
   type Move_Type is private;
   type Score_Type is range -127 .. 127;

   type Game_State_Type is record
      board       : Board_Type;
      curr_player : Player.Player_Type;
   end record;

   type Spot_Move_Score is record
      move      : Move_Type;
      est_score : Score_Type;
      exact     : Boolean := False;
   end record;

   type Compressed_Board is range 0 .. 2**63 - 1;

   function Initialize return Game_State_Type;
   function Is_Legal_Move (m : Move_Type) return Boolean;
   function Is_Legal_Board (b : Game_State_Type) return Boolean;
   function Game_Over (b : Game_State_Type) return Boolean;
   function Winner (b : Game_State_Type) return Winner_Type
   with Pre => Game_Over (b);
   type Move_List is array (Integer range <>) of Move_Type;
   function Every_Move (b : Game_State_Type) return Move_List;
   function To_String (b : Game_State_Type) return String;

   function Compress (b : Game_State_Type) return Compressed_Board
   with Pre => Is_Legal_Board (b);
   function Decompress (cb : Compressed_Board) return Game_State_Type
   with Post => Is_Legal_Board (Decompress'Result);

private

   type Piece_Count is range 0 .. 72;
   type Board_Spot is range 1 .. 12;
   type Move_Type is new Board_Spot;
   type Board_Board_Type is array (Board_Spot) of Piece_Count;
   type Board_Store_Type is array (Player_Type) of Piece_Count;
   type Board_Type is record
      Board : Board_Board_Type;
      Store : Board_Store_Type;
   end record;

end Board;
