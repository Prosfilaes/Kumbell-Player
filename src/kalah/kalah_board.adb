with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Kalah_Board is
   Total_Piece_Number : constant Piece_Count := Piece_Count'Last;

   function Board_Sum (b : Game_State_Type) return Integer;

   function Initialize return Game_State_Type is
      new_board : Game_State_Type;
   begin
      new_board.board.board :=
        Board_Board_Type'(others => Piece_Count (pieces_per_pod));
      new_board.board.store := Board_Store_Type'(0, 0);
      new_board.curr_player := 1;
      return new_board;
   end;

   function Is_Legal_Move (b : Game_State_Type; m : Move_Type) return Boolean
   is
   begin
      if b.board.board (Board_Spot (m)) = 0 then
         return false;
      elsif b.curr_player = 1 and then m > Move_Type (board_length) then
         return false;
      elsif b.curr_player = 2 and then m <= Move_Type (board_length) then
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
            new_spot := Board_Spot (board_length) * 2;
         elsif new_spot = Board_Spot (board_length + 1) then
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

      if ((new_b.curr_player = 1 and new_spot <= Board_Spot (board_length))
          or (new_b.curr_player = 2 and new_spot > Board_Spot (board_length)))
        and new_b.board.board (new_spot) = 1
      then
         declare
            other_spot : constant Board_Spot :=
              (Board_Spot (board_length) * 2 + 1) - new_spot;
         begin
            new_b.board.store (1) := @ + new_b.board.board (other_spot);
            new_b.board.board (other_spot) := 0;
         end;
      end if;
      new_b.curr_player := Next (@);
      return new_b;
   end Move;

   function Game_Over (b : Game_State_Type) return Boolean is
   begin
      if b.board.store (1) > Total_Piece_Number / 2
        or else b.board.store (2) > Total_Piece_Number / 2
      then
         return true;
      end if;
      if b.curr_player = 1 then
         for i in 1 .. Board_Spot (board_length) loop
            if b.board.board (i) /= 0 then
               return False;
            end if;
         end loop;
         return True;
      else
         for i
           in Board_Spot (1 + board_length) .. Board_Spot (board_length * 2)
         loop
            if b.board.board (i) /= 0 then
               return False;
            end if;
         end loop;
         return True;
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
      if player1 = Total_Piece_Number / 2
        and then player2 = Total_Piece_Number / 2
      then
         return 0;
      end if;
      if b.curr_player = 1 then
         for i
           in Board_Spot (board_length + 1) .. Board_Spot (board_length * 2)
         loop
            player2 := @ + b.board.board (i);
         end loop;
      else
         for i in 1 .. Board_Spot (board_length) loop
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
      for i in Move_Type'(1) .. Move_Type (board_length * 2) loop
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
      for i in Board_Spot'(1) .. Board_Spot (board_length * 2) loop
         if i = 1 or else i = Board_Spot (board_length + 1) then
            Append (s, " | ");
         end if;
         Append (s, b.board.board (i)'Image & " ");
      end loop;
      return To_String (s);
   end To_String;

   function Board_Sum (b : Game_State_Type) return Integer is
      sum : Integer := 0;
   begin
      for i in Board_Spot'(1) .. Board_Spot (board_length * 2) loop
         sum := @ + Integer (b.board.board (i));
      end loop;
      sum := @ + Integer (b.board.store (1)) + Integer (b.board.store (2));
      return sum;
   end Board_Sum;

   function Is_Legal_Board (b : Game_State_Type) return Boolean is
   begin
      return Piece_Count (Board_Sum (b)) = Total_Piece_Number;
   end Is_Legal_Board;

   function Move_Type_from_String (s : String) return Move_Type is
   begin
      return Move_Type'Value (s);
   end Move_Type_from_String;

   subtype Index is Natural range 0 .. 100;
   type Binom_Table_Type is array (Index, Index) of Compressed_Board;

   -- Precomputed table of Binomial coefficients
   Binom_Table : Binom_Table_Type;

   -- Fill Pascal’s triangle
   procedure Init_Binom is
   begin
      for n in Index loop
         Binom_Table (n, 0) := 1;
         Binom_Table (n, n) := 1;
      end loop;

      for n in Index loop
         if n >= 2 then
            -- minimum where k = 1 .. n-1 is valid
            for k in 1 .. n - 1 loop
               Binom_Table (n, k) :=
                 Binom_Table (n - 1, k - 1) + Binom_Table (n - 1, k);
            end loop;
         end if;
      end loop;
   end Init_Binom;

   Initialized : Boolean := False;

   function Binomial (N, K : Index) return Compressed_Board is
   begin
      if not Initialized then
         Init_Binom;
         Initialized := True;
      end if;

      if K > N then
         return 0;
      else
         return Binom_Table (N, K);
      end if;
   end Binomial;

   function Compress (b : Game_State_Type) return Compressed_Board is
      Rank_Value : Compressed_Board := 0;
      Total      : Natural := pieces_per_pod * 2 * board_length;
      Holes_Left : Natural := 2 * board_length + 2;
      Config     : array (1 .. 2 * board_length + 2) of Piece_Count;
   begin
      Config (1) := b.board.store (1);
      Config (2) := b.board.store (2);
      for i in 1 .. 2 * board_length loop
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
      Rank_Value := @ * 2 + (if b.curr_player = 2 then 1 else 0);
      return Rank_Value;
   end Compress;

   function Decompress (cb : Compressed_Board) return Game_State_Type is
      Num_Holes  : constant Natural := 2 + 2 * board_length;
      Total      : Natural := pieces_per_pod * 2 * board_length;
      Holes_Left : Natural := Num_Holes;
      Config     : array (1 .. Num_Holes) of Piece_Count;
      Count      : Natural;
      Comb       : Compressed_Board;
      Rank_Value : Compressed_Board := cb;
      B          : Game_State_Type;
   begin
      if Rank_Value mod 2 = 0 then
         B.curr_player := 1;
      else
         B.curr_player := 2;
      end if;
      Rank_Value := @ / 2;
      for I in 1 .. Num_Holes - 1 loop
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
      Config (Num_Holes) := Piece_Count (Total);
      B.board.store (1) := Config (1);
      B.board.store (2) := Config (2);
      for i in 1 .. board_length * 2 loop
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

   function To_String (m : Move_Type) return String is
   begin
      return m'Image;
   end To_String;

   function Base_Boards return Board_Vectors.Vector is
      ml : Board_Vectors.Vector;

      procedure Add_Chunk_Rec
        (b : Game_State_Type; depth : Integer; min_spot : Board_Spot)
      is
         new_b : Game_State_Type;
      begin
         if depth = 0 then
            pragma Assert (Is_Legal_Board (b));
            if not Game_Over (b) then
               ml.Append (b);
            end if;
         else
            new_b := b;
            for i in min_spot .. Board_Spot (board_length * 2) loop
               new_b.board.board (i) := @ + 1;
               Add_Chunk_Rec (new_b, depth - 1, i);
               new_b.board.board (i) := @ - 1;
            end loop;
         end if;
      end Add_Chunk_Rec;

      procedure Add_Chunk
        (player1_score : Piece_Count; player2_score : Piece_Count)
      is
         depth : constant Integer :=
           Integer (Total_Piece_Number)
           - (Integer (player1_score) + Integer (player2_score));
         b     : Game_State_Type;
      begin
         b.curr_player := 1;
         b.board.store (1) := player1_score;
         b.board.store (2) := player2_score;
         for i in 1 .. Board_Spot (board_length * 2) loop
            b.board.board (i) := 0;
         end loop;
         Add_Chunk_Rec (b, depth, 1);
      end Add_Chunk;

   begin
      -- List all boards with up to 16 pieces on the board
      for i in Piece_Count'(1) .. 16 loop
         for j
           in Piece_Count'(Total_Piece_Number / 2)
              - i
              .. Total_Piece_Number / 2
         loop
            Add_Chunk (Total_Piece_Number - i - j, j);
         end loop;
      end loop;
      return ml;
   end Base_Boards;

end Kalah_Board;
