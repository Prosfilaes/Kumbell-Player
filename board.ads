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

   function Winner (b : Game_State) return Integer
   with Pre => Game_Over (b);

   function First_Move (b : Game_State) return Game_State;

   function Every_Move (b : Game_State) return Board_List;

   function To_String (b : Game_State) return String;

   function Is_Compressable (b : Game_State) return Boolean;

   type Compressed_Board is mod 2**64;

   function Compress (b : Game_State) return Compressed_Board
   with
     Pre  => Is_Compressable (b) and then Is_Legal_Board (b),
     Post => Compress'Result < 2**63;

   function Compress_Base64 (cb : Compressed_Board) return String
   with
     Post =>
       Compress_Base64'Result'Length = 12
       and then DeBase64 (Compress_Base64'Result) = cb;

   function DeBase64 (s : String) return Compressed_Board
   with Pre => s'Length = 12;

   function Rotate_Board
     (b : Game_State; switch_player : Boolean := False) return Game_State;

   type Score is range -127 .. 127;
   type Spot_Move_Score is record
      move      : Board_Spot;
      est_score : Score;
      exact     : Boolean := False;
   end record;
end Board;
