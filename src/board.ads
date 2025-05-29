with Player; use Player;

package Board is
   type Piece_Count is range 0 .. 72;
   type Board_Spot is range 1 .. 12;

   type Board_Type is array (Board_Spot) of Piece_Count;
   type Store_Type is array (Player.Player) of Piece_Count;

   type Game_State is record
      board       : Board_Type;
      store       : Store_Type;
      curr_player : Player.Player;
   end record;

   type Board_List is array (Integer range <>) of Game_State;

   type Score is range -127 .. 127;
   type Spot_Move_Score is record
      move      : Board_Spot;
      est_score : Score;
      exact     : Boolean := False;
   end record;

   function Initialize return Game_State;

   function Is_Legal_Move (b : Game_State; spot : Board_Spot) return Boolean;

   function Is_Legal_Board (b : Game_State) return Boolean;

   function Move (b : Game_State; spot : Board_Spot) return Game_State
   with
     Pre  => Is_Legal_Move (b, spot),
     Post =>
       b.curr_player /= Move'Result.curr_player
       and then b.store (1) <= Move'Result.store (1)
       and then b.store (2) <= Move'Result.store (2)
       and then Is_Legal_Board (Move'Result);

   function Game_Over (b : Game_State) return Boolean;

   type Winner_Type is range -1 .. 1;
   function Winner (b : Game_State) return Winner_Type
   with Pre => Game_Over (b);

   function First_Move (b : Game_State) return Game_State;

   function Every_Move (b : Game_State) return Board_List;

   function To_String (b : Game_State) return String;

   type Compressed_Board is range 0 .. 2**63 - 1;

   function Compress (b : Game_State) return Compressed_Board
   with Pre => Is_Legal_Board (b);

   function Decompress (cb : Compressed_Board) return Game_State;

   function Rotate_Board
     (b : Game_State; switch_player : Boolean := False) return Game_State;

   function Player1_Board_Pieces (b : Game_State) return Piece_Count;

   function Player2_Board_Pieces (b : Game_State) return Piece_Count;
end Board;
