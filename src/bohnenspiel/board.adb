with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Move_Book; use Move_Book;

package body Board is

   function Board_Sum (b : Game_State_Type) return Integer;

   function Initialize return Game_State_Type is
      new_board : Game_State_Type;
   begin
      new_board.board.board := Board_Board_Type'(6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6);
      new_board.board.store := Board_Store_Type'(0, 0);
      new_board.curr_player := 1;
      return new_board;
   end;

   function Is_Legal_Move (b : Game_State_Type; m : Move_Type) return Boolean is
   begin
      if b.board.board (Board_Spot(m)) = 0 then
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
      new_b    : Game_State_Type := Initialize;
      pieces   : Piece_Count;
      spot : constant Board_Spot := Board_Spot (m);
      new_spot : Board_Spot;
   begin
      new_b.board := b.board;
      new_b.curr_player := b.curr_player;

      pieces := b.board.board (spot);
      new_b.board.board (spot) := 0;
      new_spot := spot;
      for loop_count in 1 .. pieces loop
         if new_spot = 1 then
            new_spot := 12;
         else
            new_spot := new_spot - 1;
         end if;
         new_b.board.board (new_spot) := @ + 1;
      end loop;
      loop
         declare
            count : constant Piece_Count := new_b.board.board (new_spot);
         begin
            if count = 2 or else count = 4 or else count = 6 then
               new_b.board.store (new_b.curr_player) := @ + count;
               new_b.board.board (new_spot) := 0;
               if new_spot = 12 then
                  new_spot := 1;
               else
                  new_spot := new_spot + 1;
               end if;
            else
               new_b.curr_player := Next (@);
               return new_b;
            end if;
         end;
      end loop;
   end Move;

   function Game_Over (b : Game_State_Type) return Boolean is
      p : Piece_Count;
   begin
      if b.board.store (1) >= 37 or else b.board.store (2) >= 37 then
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
      if player1 > 36 then
         return -1;
      end if;
      if player2 > 36 then
         return 1;
      end if;
      if player1 = 36 and then player2 = 36 then
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
      pragma Assert (player1 + player2 = 72);
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
      begin
         for i in 1 .. Count loop
            while not Legal_Moves (Curr_Element) loop
               Curr_Element := @ + 1;
            end loop;
            Ml (i) := Curr_Element;
            Curr_Element := @ + 1;
         end loop;
         return Ml;
      end;
   end Every_Move;

   function To_String (b : Game_State_Type) return String is
      s : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (s, "P1:" & b.board.store (1)'Image & " P2:" & b.board.store (2)'Image);
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
      return Board_Sum (b) = 72;
   end Is_Legal_Board;

   function Move_Type_from_String (s : String) return Move_Type is
   begin
      return Move_Type'Value(s);
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
      Total      : Natural := 72;
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
      Total      : Natural := 72;
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
   return Board_Categories_Type(72 - b.Board.Store(1) - b.Board.Store(2));
   end Categorize;

   function Title_Line (bc : Board_Categories_Type) return String is
   begin
      return bc'Image & " pieces on the board";
   end Title_Line;

   function Player1_Board_Pieces (b : Game_State_Type) return Piece_Count is
   begin
      return b.board.board(1) + 
      b.board.board(2) + 
      b.board.board(3) + 
      b.board.board(4) + 
      b.board.board(5) + 
      b.board.board(6);
   end Player1_Board_Pieces;

   function Player2_Board_Pieces (b : Game_State_Type) return Piece_Count is
   begin
      return b.board.board(7) + 
      b.board.board(8) + 
      b.board.board(9) + 
      b.board.board(10) + 
      b.board.board(11) + 
      b.board.board(12);
   end Player2_Board_Pieces;

   function Is_Solvable (b : Game_State_Type) return Boolean is
      p1 : constant Piece_Count := Player1_Board_Pieces (b);
      p2 : constant Piece_Count := Player2_Board_Pieces (b);
   begin
      if b.curr_player = 2 then
         if p1 = 0 and b.board.board (7) /= p2 then
            return true;
         elsif b.board.board (1) = p1 and p1 <= 6 and b.board.board (7) /= p2 then
            return true;
         end if;
      else
         if p2 = 0 and b.board.board (1) /= p1 then
            return true;
         elsif b.board.board (7) = p2 and p2 <= 6 and b.board.board (1) /= p1 then
            return true;
         end if;
      end if;
      return False;
   end Is_Solvable;

   function Solve_Simple (b : Game_State_Type) return Move_Book.Move_Score_Type is
      p1, p2    : Piece_Count;
      new_store : Piece_Count;
   begin
      p1 := Player1_Board_Pieces (b);
      p2 := Player2_Board_Pieces (b);

      if b.curr_player = 2 then
         if (p1 = 0 and then b.board.board (7) /= p2)
           or else (b.board.board (1) = p1
                    and then p1 <= 6
                    and then b.board.board (7) /= p2)
         then
            for m in Board_Spot'(8) .. 12 loop
               if b.board.board (m) /= 0 then
                  -- m is effectively a null move; p1 is still 0.
                  -- Thus player 1 goes and loses because they have
                  -- no move. p2 gets added to Player 2's score.
                  new_store := b.board.store (2) + p2 + p1;
                  if b.board.store (1) > new_store then
                     return Move_Score_Type'(Move_Type(m), -1, True);
                  elsif b.board.store (1) = new_store then
                     return Move_Score_Type'(Move_Type(m), 0, True);
                  else
                     return Move_Score_Type'(Move_Type(m), 1, True);
                  end if;
               end if;
            end loop;
         end if;
      else
         if (p2 = 0 and then b.board.board (1) /= p1)
           or else (b.board.board (7) = p2
                    and then p2 <= 6
                    and then b.board.board (1) /= p1)
         then
            for m in Board_Spot'(2) .. 6 loop
               if b.board.board (m) /= 0 then
                  -- m is effectively a null move; p2 is still 0.
                  -- Thus player 2 goes and loses because they have
                  -- no move. p1 gets added to Player 1's score.
                  new_store := b.board.store (1) + p1 + p2;
                  if b.board.store (2) > new_store then
                     return Move_Score_Type'(Move_Type(m), 1, True);
                  elsif b.board.store (2) = new_store then
                     return Move_Score_Type'(Move_Type(m), 0, True);
                  else
                     return Move_Score_Type'(Move_Type(m), -1, True);
                  end if;
               end if;
            end loop;
         -- We check in Is_Simple so we don't have to call AB Search again.

         end if;
      end if;

      raise Constraint_Error with "No legal moves in degenerate position";
   end Solve_Simple;

   function To_String (m : Move_Type) return String is
   begin
   return m'Image;
   end To_String;

end Board;
