with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;
with Ada.Text_IO;

package body Board is

   function Board_Sum (b : Game_State) return Integer;

   function Initialize return Game_State is
      new_board : Game_State;
   begin
      new_board.board := Board_Type'(6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6);
      new_board.store := Store_Type'(0, 0);
      new_board.curr_player := 1;
      return new_board;
   end;

   function Is_Legal_Move (b : Game_State; spot : Board_Spot) return Boolean is
   begin
      if b.board (spot) = 0 then
         return false;
      elsif b.curr_player = 1 and then spot > 6 then
         return false;
      elsif b.curr_player = 2 and then spot < 7 then
         return false;
      else
         return true;
      end if;
   end Is_Legal_Move;

   function Move (b : Game_State; spot : Board_Spot) return Game_State is
      new_b    : Game_State := Initialize;
      pieces   : Piece_Count;
      new_spot : Board_Spot;
   begin
      new_b.board := b.board;
      new_b.store := b.store;
      new_b.curr_player := b.curr_player;
      pieces := b.board (spot);
      new_b.board (spot) := 0;
      new_spot := spot;
      for loop_count in 1 .. pieces loop
         if new_spot = 1 then
            new_spot := 12;
         else
            new_spot := new_spot - 1;
         end if;
         new_b.board (new_spot) := @ + 1;
      end loop;
      loop
         declare
            count : constant Piece_Count := new_b.board (new_spot);
         begin
            if count = 2 or else count = 4 or else count = 6 then
               new_b.store (new_b.curr_player) := @ + count;
               new_b.board (new_spot) := 0;
               if new_spot = 12 then
                  new_spot := 1;
               else
                  new_spot := new_spot + 1;
               end if;
            else
               new_b.curr_player := Next (@);
               if false then
                  if b.curr_player = new_b.curr_player then
                     Ada.Text_IO.Put_Line ("Player didn't change!");
                  elsif b.store (1) > new_b.store (1) then
                     Ada.Text_IO.Put_Line ("Player 1 store decreased!");
                     Ada.Text_IO.Put_Line ("Old: " & To_String (b));
                     Ada.Text_IO.Put_Line ("New: " & To_String (new_b));
                  elsif b.store (2) > new_b.store (2) then
                     Ada.Text_IO.Put_Line ("Player 2 store decreased!");
                     Ada.Text_IO.Put_Line ("Old: " & To_String (b));
                     Ada.Text_IO.Put_Line ("New: " & To_String (new_b));
                  elsif Board_Sum (new_b) /= 72 then
                     Ada.Text_IO.Put_Line ("Board sum isn't 72!");
                     Ada.Text_IO.Put_Line ("Old: " & To_String (b));
                     Ada.Text_IO.Put_Line ("New: " & To_String (new_b));
                  elsif not Is_Legal_Board (new_b) then
                     -- Is_legal Board current only does board sum
                     Ada.Text_IO.Put_Line ("Generic Board isn't legal!");
                     Ada.Text_IO.Put_Line ("Old: " & To_String (b));
                     Ada.Text_IO.Put_Line ("New: " & To_String (new_b));
                  end if;
               end if;
               return new_b;
            end if;
         end;
      end loop;
   end Move;

   function Game_Over (b : Game_State) return Boolean is
   begin
      if b.store (1) >= 37 or else b.store (2) >= 37 then
         return true;
      end if;
      if b.curr_player = 1 then
         for i in Board_Spot'(1) .. 6 loop
            if b.board (i) /= 0 then
               return false;
            end if;
         end loop;
      else
         for i in Board_Spot'(7) .. 12 loop
            if b.board (i) /= 0 then
               return false;
            end if;
         end loop;
      end if;
      return true;
   end Game_Over;

   function Winner (b : Game_State) return Integer is
      --            with Pre => Game_Over (b);
      player1 : Piece_Count := b.store (1);
      player2 : Piece_Count := b.store (2);
   begin
      if b.curr_player = 1 then
         for i in Board_Spot'(1) .. 6 loop
            player2 := @ + b.board (i);
         end loop;
      else
         for i in Board_Spot'(7) .. 12 loop
            player1 := @ + b.board (i);
         end loop;
      end if;
      if player1 > player2 then
         return -1;
      elsif player1 < player2 then
         return 1;
      else
         return 0;
      end if;
   end Winner;

   function First_Move (b : Game_State) return Game_State is
   begin
      for i in Board_Spot'(1) .. 12 loop
         if Is_Legal_Move (b, i) then
            return Move (b, i);
         end if;
      end loop;
   end First_Move;

   function Every_Move (b : Game_State) return Board_List is
      Legal_Moves : array (Board_Spot) of Boolean := [others => False];
      Count       : Integer := 0;
   begin
      for i in Board_Spot'(1) .. 12 loop
         if Is_Legal_Move (b, i) then
            Legal_Moves (i) := True;
            Count := Count + 1;
         end if;
      end loop;
      declare
         Bl           : Board_List (1 .. Count);
         Curr_Element : Board_Spot := 1;
      begin
         for i in 1 .. Count loop
            while not Legal_Moves (Curr_Element) loop
               Curr_Element := @ + 1;
            end loop;
            Bl (i) := Move (b, Curr_Element);
         end loop;
         return Bl;
      end;
   end Every_Move;

   function To_String (b : Game_State) return String is
      s : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (s, "--->" & Ada.Characters.Latin_1.LF);
      for i in Board_Spot'(1) .. 6 loop
         Append (s, b.board (i)'Image & " ");
      end loop;
      Append (s, Ada.Characters.Latin_1.LF);
      Append (s, "<---" & Ada.Characters.Latin_1.LF);
      for i in reverse Board_Spot'(7) .. 12 loop
         Append (s, b.board (i)'Image & " ");
      end loop;
      Append (s, Ada.Characters.Latin_1.LF);
      Append (s, "Player 1: ");
      Append (s, b.store (1)'Image);
      Append (s, " Player 2: ");
      Append (s, b.store (2)'Image);
      Append (s, " Player to move: ");
      Append (s, b.curr_player'Image);
      Append (s, Ada.Characters.Latin_1.LF);
      return To_String (s);
   end To_String;

   function Board_Sum (b : Game_State) return Integer is
      sum : Integer := 0;
   begin
      for i in Board_Spot'(1) .. 12 loop
         sum := @ + Integer (b.board (i));
      end loop;
      sum := @ + Integer (b.store (1)) + Integer (b.store (2));
      return sum;
   end Board_Sum;

   function Is_Legal_Board (b : Game_State) return Boolean is
   begin
      return Board_Sum (b) = 72;
   end Is_Legal_Board;

   function Rotate_Board
     (b : Game_State; switch_player : Boolean := False) return Game_State
   is
      new_board : Game_State := Initialize;
   begin
      for i in Board_Spot'(1) .. 6 loop
         new_board.board (i) := b.board (6 + i);
         new_board.board (6 + i) := b.board (i);
      end loop;
      new_board.store (1) := b.store (2);
      new_board.store (2) := b.store (1);
      if switch_player then
         new_board.curr_player := Next (b.curr_player);
      else
         new_board.curr_player := b.curr_player;
      end if;
      return new_board;
   end Rotate_Board;

   function Is_Compressable (b : Game_State) return Boolean is
   begin
      for i in Board_Spot'(1) .. 12 loop
         if b.board (i) > 15 then
            return false;
         end if;
      end loop;
      return true;
   end Is_Compressable;

   function Compress (b : Game_State) return Compressed_Board is
      compressed : Compressed_Board := 0;
      new_b      : Game_State := b;
   begin
      if new_b.curr_player = 2 then
         new_b := Rotate_Board (new_b, True);
      end if;
      for i in Board_Spot'(1) .. 12 loop
         compressed := compressed * 16 + Compressed_Board (new_b.board (i));
      end loop;
      compressed := compressed * 64 + Compressed_Board (b.store (1));
      compressed := compressed * 64 + Compressed_Board (b.store (2));
      return compressed;
   end Compress;

   function Compress_Base64 (cb : Compressed_Board) return String is
      compressed : Compressed_Board := cb;
      s          : Unbounded_String;
      base64     : constant String :=
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
   begin
      for i in 1 .. 12 loop
         append (s, base64 (Integer (compressed mod 64 + 1)));
         compressed := compressed / 64;
      end loop;
      return To_String (s);
   end Compress_Base64;

   function DeBase64 (s : String) return Compressed_Board is
   begin
      declare
         compressed : Compressed_Board := 0;
      begin
         for i in reverse 1 .. 12 loop
            if s (i) >= 'A' and then s (i) <= 'Z' then
               compressed :=
                 compressed
                 * 64
                 + Compressed_Board
                     (Character'Pos (s (i)) - Character'Pos ('A'));
            elsif s (i) >= 'a' and then s (i) <= 'z' then
               compressed :=
                 compressed
                 * 64
                 + Compressed_Board
                     (Character'Pos (s (i)) - Character'Pos ('a') + 26);
            elsif s (i) >= '0' and then s (i) <= '9' then
               compressed :=
                 compressed
                 * 64
                 + Compressed_Board
                     (Character'Pos (s (i)) - Character'Pos ('0') + 52);
            elsif s (i) = '+' then
               compressed := compressed * 64 + Compressed_Board (62);
            elsif s (i) = '/' then
               compressed := compressed * 64 + Compressed_Board (63);
            else
               Ada.Text_IO.Put_Line
                 ("Illegal character in base64 string: " & s (i)'Image);
               raise Constraint_Error;
            end if;
         end loop;
         return compressed;
      end;
   end DeBase64;

end Board;
