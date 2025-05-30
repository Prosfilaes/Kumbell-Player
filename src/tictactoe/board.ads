with Player; use Player;

package Board is

   type Board_Type is private;
   type Move_Type is private;
   type Score_Type is range -127 .. 127;

   type Game_State_Type is record
      board       : Board_Type;
      curr_player : Player.Player_Type;
   end record;

   type Compressed_Board is range 0 .. 2**63 - 1;

   function Initialize return Game_State_Type;
   function Is_Legal_Move (b : Game_State_Type; m : Move_Type) return Boolean;
   function Is_Legal_Board (b : Game_State_Type) return Boolean;
   function Game_Over (b : Game_State_Type) return Boolean;
   function Winner (b : Game_State_Type) return Winner_Type
   with Pre => Game_Over (b);
   type Move_List is array (Integer range <>) of Move_Type;
   function Every_Move (b : Game_State_Type) return Move_List;
   function Move (b : Game_State_Type; m : Move_Type) return Game_State_Type;
   function To_String (b : Game_State_Type) return String;
   function To_String (m : Move_Type) return String;
   function Move_Type_from_String (s : String) return Move_Type;

   function Compress (b : Game_State_Type) return Compressed_Board
   with Pre => Is_Legal_Board (b);
   function Decompress (cb : Compressed_Board) return Game_State_Type
   with Post => Is_Legal_Board (Decompress'Result);

   type Board_Categories_Type is range 0 .. 9;
   function Categorize (b : Game_State_Type) return Board_Categories_Type;
   function Title_Line (bc : Board_Categories_Type) return String;

private

   --type Piece_Count is range 0 .. 72;
   type Board_Spot is range 1 .. 9;
   type Move_Type is new Board_Spot;
   type Marker is range 0 .. 2;
   type Board_Type is array (Board_Spot) of Marker;

end Board;
