with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Containers; use Ada.Containers;

package body Board is
   Total_Piece_Number : constant := Piece_Count'Last;

   function Board_Sum (b : Game_State_Type) return Integer;

   function Initialize return Game_State_Type is
      new_board : Game_State_Type;
   begin
      new_board.board.board :=
        Board_Board_Type'(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3);
      new_board.board.store := Board_Store_Type'(0, 0);
      new_board.curr_player := 1;
      return new_board;
   end;

   function Is_Legal_Move (b : Game_State_Type; m : Move_Type) return Boolean
   is
   begin
      if b.board.board (Board_Spot (m)) = 0 then
         return false;
      elsif b.curr_player = 1 and then m > 6 then
         return false;
      elsif b.curr_player = 2 and then m < 7 then
         return false;
      else
         return true;
      end if;
   end Is_Legal_Move;

   function Move (b : Game_State_Type; m : Move_Type) return Game_State_Type is
      new_b      : Game_State_Type := Initialize;
      pieces     : Piece_Count;
      spot       : constant Board_Spot := Board_Spot (m);
      new_spot   : Board_Spot;
      loop_count : Piece_Count;
   begin
      new_b.board := b.board;
      new_b.curr_player := b.curr_player;

      pieces := b.board.board (spot);
      new_b.board.board (spot) := 0;
      new_spot := spot;
      loop_count := 1;
      while loop_count <= pieces loop
         if new_spot = 1 then
            if new_b.curr_player = 1 then
               new_b.board.store (1) := @ + 1;
               loop_count := @ + 1;
               if (loop_count > pieces) then
                  return new_b; -- Player gets to take another turn

               end if;
            end if;
            new_spot := 12;
         elsif new_spot = 7 then
            if new_b.curr_player = 2 then
               new_b.board.store (2) := @ + 1;
               loop_count := @ + 1;
               if (loop_count > pieces) then
                  return new_b; -- Player gets to take another turn

               end if;
            end if;
            new_spot := @ - 1;
         else
            new_spot := @ - 1;
         end if;
         new_b.board.board (new_spot) := @ + 1;
         loop_count := @ + 1;
      end loop;

      if ((new_b.curr_player = 1 and new_spot < 7)
          or (new_b.curr_player = 2 and new_spot > 6))
        and new_b.board.board (new_spot) = 1
      then
         declare
            other_spot : constant Board_Spot := 13 - new_spot;
         begin
            new_b.board.store (1) := @ + new_b.board.board (other_spot);
            new_b.board.board (other_spot) := 0;
         end;
      end if;
      new_b.curr_player := Next (@);
      return new_b;
   end Move;

   function Game_Over (b : Game_State_Type) return Boolean is
      p : Piece_Count;
   begin
      if b.board.store (1) >= 25 or else b.board.store (2) >= 25 then
         return true;
      end if;
      if b.curr_player = 1 then
         p :=
           b.board.board (1)
           + b.board.board (2)
           + b.board.board (3)
           + b.board.board (4)
           + b.board.board (5)
           + b.board.board (6);
         return p = 0;
      else
         p :=
           b.board.board (7)
           + b.board.board (8)
           + b.board.board (9)
           + b.board.board (10)
           + b.board.board (11)
           + b.board.board (12);
         return p = 0;
      end if;
   end Game_Over;

   function Winner (b : Game_State_Type) return Winner_Type is
      --            with Pre => Game_Over (b);
      player1 : Piece_Count := b.board.store (1);
      player2 : Piece_Count := b.board.store (2);
   begin
      if player1 > Total_Piece_Number / 2 then
         return -1;
      end if;
      if player2 > Total_Piece_Number / 2 then
         return 1;
      end if;
      if player1 = Total_Piece_Number / 2 and then player2 = Total_Piece_Number / 2 then
         return 0;
      end if;
      if b.curr_player = 1 then
         for i in Board_Spot'(7) .. 12 loop
            player2 := @ + b.board.board (i);
         end loop;
      else
         for i in Board_Spot'(1) .. 6 loop
            player1 := @ + b.board.board (i);
         end loop;
      end if;
      pragma Assert (player1 + player2 = Total_Piece_Number);
      if player1 > player2 then
         return -1;
      elsif player1 < player2 then
         return 1;
      else
         return 0;
      end if;
   end Winner;

   function Every_Move (b : Game_State_Type) return Move_List is
      Legal_Moves : array (Move_Type) of Boolean := [others => False];
      Count       : Integer := 0;
   begin
      for i in Move_Type'(1) .. 12 loop
         if Is_Legal_Move (b, i) then
            Legal_Moves (i) := True;
            Count := Count + 1;
         end if;
      end loop;
      declare
         Ml           : Move_List (1 .. Count);
         Curr_Element : Move_Type := 1;
         i            : Integer := 1;
      begin
         while true loop
            while not Legal_Moves (Curr_Element) loop
               Curr_Element := @ + 1;
            end loop;
            Ml (i) := Curr_Element;
            if i = Count then
               return Ml;
            end if;
            Curr_Element := @ + 1;
            i := @ + 1;
         end loop;
         return Ml;
      end;
   end Every_Move;

   function To_String (b : Game_State_Type) return String is
      s : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append
        (s,
         "P1:" & b.board.store (1)'Image & " P2:" & b.board.store (2)'Image);
      Append (s, " Curr:" & b.curr_player'Image);
      for i in Board_Spot'(1) .. 12 loop
         if i = 1 or else i = 7 then
            Append (s, " | ");
         end if;
         Append (s, b.board.board (i)'Image & " ");
      end loop;
      return To_String (s);
   end To_String;

   function Board_Sum (b : Game_State_Type) return Integer is
      sum : Integer := 0;
   begin
      for i in Board_Spot'(1) .. 12 loop
         sum := @ + Integer (b.board.board (i));
      end loop;
      sum := @ + Integer (b.board.store (1)) + Integer (b.board.store (2));
      return sum;
   end Board_Sum;

   function Is_Legal_Board (b : Game_State_Type) return Boolean is
   begin
      return Board_Sum (b) = Total_Piece_Number;
   end Is_Legal_Board;

   function Move_Type_from_String (s : String) return Move_Type is
   begin
      return Move_Type'Value (s);
   end Move_Type_from_String;

   function Binomial (N, K : Natural) return Compressed_Board is
      Result : Compressed_Board := 1;
   begin
      if K > N then
         return 0;
      end if;

      for i in 1 .. K loop
         Result :=
           Result * Compressed_Board (N - i + 1) / Compressed_Board (i);
      end loop;

      return Result;
   end Binomial;

   function Compress (b : Game_State_Type) return Compressed_Board is
      Rank_Value : Compressed_Board := 0;
      Total      : Natural := 36;
      Holes_Left : Natural := 14;
      Config     : array (1 .. 14) of Piece_Count;
   begin
      if b.curr_player = 2 then
         raise Constraint_Error
           with "Compress: Player 2 is not allowed to compress";
      end if;
      Config (1) := b.board.store (1);
      Config (2) := b.board.store (2);
      for i in 1 .. 12 loop
         Config (i + 2) := b.board.board (Board_Spot (i));
      end loop;
      for I in 1 .. 13 loop
         Holes_Left := Holes_Left - 1;
         for X in 0 .. Integer (Config (I)) - 1 loop
            Rank_Value :=
              Rank_Value
              + Binomial (Total - X - 1 + Holes_Left, Holes_Left - 1);
         end loop;
         Total := Total - Integer (Config (I));
      end loop;
      return Rank_Value;
   end Compress;

   function Decompress (cb : Compressed_Board) return Game_State_Type is
      Total      : Natural := 36;
      Holes_Left : Natural := 14;
      Config     : array (1 .. 14) of Piece_Count;
      Count      : Natural;
      Comb       : Compressed_Board;
      Rank_Value : Compressed_Board := cb;
      B          : Game_State_Type;
   begin
      for I in 1 .. 13 loop
         Count := 0;
         loop
            Comb :=
              Binomial (Total - Count - 1 + Holes_Left - 1, Holes_Left - 2);
            exit when Comb > Rank_Value;
            Rank_Value := Rank_Value - Comb;
            Count := Count + 1;
         end loop;
         Config (I) := Piece_Count (Count);
         Total := Total - Count;
         Holes_Left := Holes_Left - 1;
      end loop;
      Config (14) := Piece_Count (Total);
      B.curr_player := 1;
      B.board.store (1) := Config (1);
      B.board.store (2) := Config (2);
      for i in 1 .. 12 loop
         B.board.board (Board_Spot (i)) := Config (i + 2);
      end loop;
      return B;
   end Decompress;


   function Categorize (b : Game_State_Type) return Board_Categories_Type is
   begin
      return
        Board_Categories_Type
          (Total_Piece_Number - b.Board.Store (1) - b.Board.Store (2));
   end Categorize;

   function Title_Line (bc : Board_Categories_Type) return String is
   begin
      return bc'Image & " pieces on the board";
   end Title_Line;

   function Player1_Board_Pieces (b : Game_State_Type) return Piece_Count is
   begin
      return
        b.board.board (1)
        + b.board.board (2)
        + b.board.board (3)
        + b.board.board (4)
        + b.board.board (5)
        + b.board.board (6);
   end Player1_Board_Pieces;

   function Player2_Board_Pieces (b : Game_State_Type) return Piece_Count is
   begin
      return
        b.board.board (7)
        + b.board.board (8)
        + b.board.board (9)
        + b.board.board (10)
        + b.board.board (11)
        + b.board.board (12);
   end Player2_Board_Pieces;

   function To_String (m : Move_Type) return String is
   begin
      return m'Image;
   end To_String;

   function Hash (b : Game_State_Type) return Ada.Containers.Hash_Type is
      ht : Ada.Containers.Hash_Type;
   begin
      ht := Ada.Containers.Hash_Type (b.board.store(1));
      for i in Board_Spot'(1) .. 12 loop
      ht := 3 * @ + Ada.Containers.Hash_Type (b.board.board(i));
      end loop;
      return ht;
   end Hash;

   -- Fallback
   --function Hash (b : Game_State_Type) return Ada.Containers.Hash_Type is
   --   type Board_Bytes is mod 2**64;
   --   cb : constant Board_Bytes := Board_Bytes (Compress (b));
   --begin
   --   return Ada.Containers.Hash_Type (cb mod 2**32 xor (cb / 2**32));
   --end Hash;

end Board;
