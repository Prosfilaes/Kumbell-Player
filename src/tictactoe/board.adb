with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Move_Book;             use Move_Book;
with Ada.Text_IO;           use Ada.Text_IO;

package body Board is

   function Initialize return Game_State_Type is
      new_board : Game_State_Type;
   begin
      new_board.board := Board_Type'(0, 0, 0, 0, 0, 0, 0, 0, 0);
      new_board.curr_player := 1;
      return new_board;
   end;

   function Is_Legal_Move (b : Game_State_Type; m : Move_Type) return Boolean
   is
   begin
      return b.board (Board_Spot (m)) = 0;
   end Is_Legal_Move;

   function Move (b : Game_State_Type; m : Move_Type) return Game_State_Type is
      new_b : Game_State_Type;
   begin
      new_b.board := b.board;
      new_b.board (Board_Spot (m)) := Marker (b.curr_player);
      new_b.curr_player := Next (b.curr_player);

      return new_b;
   end Move;

   function Game_Over (b : Game_State_Type) return Boolean is
      victory  : Boolean;
      complete : Boolean;
   begin
      victory :=
        (b.board (1) /= 0
         and ((b.board (1) = b.board (2) and b.board (2) = b.board (3))
              or (b.board (1) = b.board (5) and b.board (5) = b.board (9))
              or (b.board (1) = b.board (4) and b.board (4) = b.board (7))))
        or (b.board (2) /= 0
            and (b.board (2) = b.board (5) and b.board (5) = b.board (8)))
        or (b.board (3) /= 0
            and ((b.board (3) = b.board (6) and b.board (6) = b.board (9))
                 or (b.board (3) = b.board (5) and b.board (5) = b.board (7))))
        or (b.board (4) /= 0
            and (b.board (4) = b.board (5) and b.board (5) = b.board (6)))
        or (b.board (7) /= 0
            and (b.board (7) = b.board (8) and b.board (8) = b.board (9)));
      complete :=
        b.board (1) /= 0
        and b.board (2) /= 0
        and b.board (3) /= 0
        and b.board (4) /= 0
        and b.board (5) /= 0
        and b.board (6) /= 0
        and b.board (7) /= 0
        and b.board (8) /= 0
        and b.board (9) /= 0;
      return victory or complete;
   end Game_Over;

   function Winner (b : Game_State_Type) return Winner_Type is
      --            with Pre => Game_Over (b);
   begin
      if (b.board (1) = 1
          and ((b.board (1) = b.board (2) and b.board (2) = b.board (3))
               or (b.board (1) = b.board (5) and b.board (5) = b.board (9))
               or (b.board (1) = b.board (4) and b.board (4) = b.board (7))))
        or (b.board (2) = 1
            and (b.board (2) = b.board (5) and b.board (5) = b.board (8)))
        or (b.board (3) = 1
            and ((b.board (3) = b.board (6) and b.board (6) = b.board (9))
                 or (b.board (3) = b.board (5) and b.board (5) = b.board (7))))
        or (b.board (4) = 1
            and (b.board (4) = b.board (5) and b.board (5) = b.board (6)))
        or (b.board (7) = 1
            and (b.board (7) = b.board (8) and b.board (8) = b.board (9)))
      then
         return -1;
      elsif (b.board (1) = 2
             and ((b.board (1) = b.board (2) and b.board (2) = b.board (3))
                  or (b.board (1) = b.board (5) and b.board (5) = b.board (9))
                  or (b.board (1) = b.board (4)
                      and b.board (4) = b.board (7))))
        or (b.board (2) = 2
            and (b.board (2) = b.board (5) and b.board (5) = b.board (8)))
        or (b.board (3) = 2
            and ((b.board (3) = b.board (6) and b.board (6) = b.board (9))
                 or (b.board (3) = b.board (5) and b.board (5) = b.board (7))))
        or (b.board (4) = 2
            and (b.board (4) = b.board (5) and b.board (5) = b.board (6)))
        or (b.board (7) = 2
            and (b.board (7) = b.board (8) and b.board (8) = b.board (9)))
      then
         return 1;
      else
         return 0;
      end if;
   end Winner;

   function Every_Move (b : Game_State_Type) return Move_List is
      Count : Integer := 0;
   begin
      for i in Board_Spot'(1) .. 9 loop
         if b.board (i) = 0 then
            Count := @ + 1;
         end if;
      end loop;
      declare
         Ml           : Move_List (1 .. Count);
         Curr_Element : Move_Type := 1;
      begin
         for i in 1 .. Count loop
            while b.board (Board_Spot (Curr_Element)) /= 0 loop
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
      Append (s, " Curr:" & b.curr_player'Image);
      for i in Board_Spot'(1) .. 9 loop
         if i = 4 or else i = 7 then
            Append (s, " | ");
         end if;
         Append (s, b.board (i)'Image & " ");
      end loop;
      return To_String (s);
   end To_String;

   function Parity (b : Game_State_Type) return Integer is
      counter : Integer := 0;
   begin
      for i in Board_Spot loop
         if b.board (i) = 1 then
            counter := @ + 1;
         elsif b.board (i) = 2 then
            counter := @ - 1;
         end if;
      end loop;
      return counter;
   end Parity;

   function Is_Legal_Board (b : Game_State_Type) return Boolean is
      counter : constant Integer := Parity (b);
   begin
      return
        (counter = 0 and b.curr_player = 1)
        or (counter = 1 and b.curr_player = 2);
   end Is_Legal_Board;

   function Move_Type_from_String (s : String) return Move_Type is
   begin
      return Move_Type'Value (s);
   end Move_Type_from_String;

   function Compress (b : Game_State_Type) return Compressed_Board is
      total : Compressed_Board := 0;
   begin
      for i in Board_Spot'(1) .. 9 loop
         total := @ * 4 + Compressed_Board (b.board(i));
      end loop;
      return total;
   end Compress;

   function Decompress (cb : Compressed_Board) return Game_State_Type is
      Comb : Compressed_Board := cb;
      B    : Game_State_Type;
      Pb   : Integer;
   begin
      for I in reverse Board_Spot'(1) .. 9 loop
         B.board (I) := Marker (Comb mod 4);
         Comb := Comb / 4;
      end loop;
      Pb := Parity (B);
      pragma Assert (Pb = 0 or Pb = 1);
      if Parity (B) = 0 then
         B.curr_player := 1;
      else
         B.curr_player := 2;
      end if;
      return B;
   end Decompress;

   function Categorize (b : Game_State_Type) return Board_Categories_Type is
      count : Integer := 0;
   begin
      for I in Board_Spot loop
         if b.board (I) /= 0 then
            count := @ + 1;
         end if;
      end loop;
      return Board_Categories_Type (count);
   end Categorize;

   function Title_Line (bc : Board_Categories_Type) return String is
   begin
      return bc'Image & " pieces on the board";
   end Title_Line;

   function To_String (m : Move_Type) return String is
   begin
      return m'Image;
   end To_String;

end Board;
