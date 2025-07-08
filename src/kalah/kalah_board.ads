with Player; use Player;

generic
   pieces_per_pod : in Positive;
   board_length : in Positive;
   misere : in Boolean := False;
package Kalah_Board is
   -- These should be private, but it won't let me create a vector type if they're private
   type Piece_Count is
     new Natural range 0 .. pieces_per_pod * board_length * 2;
   type Board_Spot is new Positive range 1 .. board_length * 2;
   type Move_Type is new Board_Spot;
   type Board_Board_Type is array (Board_Spot) of Piece_Count;
   type Board_Store_Type is array (Player_Type) of Piece_Count;
   -- End private types
   type Board_Type is record
      board : Board_Board_Type;
      store : Board_Store_Type;
   end record;

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
   with Pre => Game_Over (b), Inline;

   type Move_List is array (Integer range <>) of Move_Type;
   function Every_Move (b : Game_State_Type) return Move_List;
   function Move (b : Game_State_Type; m : Move_Type) return Game_State_Type;
   function To_String (b : Game_State_Type) return String;
   function To_String (m : Move_Type) return String;
   function Move_Type_from_String (s : String) return Move_Type;

   -- Compress converts it to a 63 bit number; these are generous limits to keep
   -- the board size such that it can be so converted. If pressing these limits
   -- is necessary, more calculations can refine them.
   function Compress (b : Game_State_Type) return Compressed_Board
   with
     Pre =>
       ((board_length <= 6 and then pieces_per_pod <= 12)
        or (board_length <= 8 and then pieces_per_pod <= 5))
       and then Is_Legal_Board (b);

   function Decompress (cb : Compressed_Board) return Game_State_Type
   with Post => Is_Legal_Board (Decompress'Result);

   type Board_Categories_Type is range 0 .. 36;
   function Categorize (b : Game_State_Type) return Board_Categories_Type;
   function Title_Line (bc : Board_Categories_Type) return String;
   --   function Hash (b : Game_State_Type) return Ada.Containers.Hash_Type;

   type Game_State_Consumer is access procedure (B : in Game_State_Type);

   procedure Base_Boards
     (num_board_piece : Piece_Count;
      consumer        : Game_State_Consumer);

end Kalah_Board;
