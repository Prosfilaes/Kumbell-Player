with Ada.Integer_Text_IO;
with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;
with Exact_AB;

package body Move_Book is
   function Hash (b : Game_State) return Ada.Containers.Hash_Type is
      cb : constant Compressed_Board := Compress (b);
   begin
      return Ada.Containers.Hash_Type (cb mod 2**32 xor (cb / 2**32));
   end Hash;

   function Is_Equal (a, b : Game_State) return Boolean is
   begin
      return a = b;
   end Is_Equal;

   package Move_Hash_Map is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => Game_State,
        Element_Type    => Spot_Move_Score,
        Hash            => Hash,
        Equivalent_Keys => Is_Equal);

   Game_Book : Move_Hash_Map.Map;

   package Move_Set is new
     Ada.Containers.Hashed_Sets
       (Element_Type        => Game_State,
        Hash                => Hash,
        Equivalent_Elements => Is_Equal);
   Unknown_Move_Book : Move_Set.Set;

   function Get_Spot_Move_Score (s : String) return Spot_Move_Score is
      sms       : Spot_Move_Score;
      spot_move : Board_Spot;
      new_score : Score;
      start_pos : Positive := 1;
      i         : Integer;
   begin
      Ada.Integer_Text_IO.Get (s, i, start_pos);
      new_score := Score (i);
      Ada.Integer_Text_IO.Get (s (start_pos + 1 .. s'Last), i, start_pos);
      spot_move := Board_Spot (i);
      sms.move := spot_move;
      sms.exact := True;
      case new_score is
         when 1 =>
            sms.est_score := 127;

         when 2 =>
            sms.est_score := -127;

         when 0 =>
            sms.est_score := 0;

         when others =>
            raise Constraint_Error
              with "Invalid score value: " & new_score'Image;
      end case;
      return sms;
   end Get_Spot_Move_Score;

   procedure Load_Book (filename : String) is
   begin
      declare
         File : Ada.Text_IO.File_Type;
      begin
         Ada.Text_IO.Open (File, Ada.Text_IO.In_File, filename);
         while not Ada.Text_IO.End_Of_File (File) loop
            declare
               Line : constant string := Ada.Text_IO.Get_Line (File);
               cb   : Compressed_Board;
               sms  : Spot_Move_Score;
               b    : Game_State;
            begin
               -- Ignore all lines that don't fit the format
               if Line'Length >= 14
                 and then ((Line (1) >= '0' and then Line (1) <= '9')
                           or else (Line (1) >= 'A' and then Line (1) <= 'Z')
                           or else (Line (1) >= 'a' and then Line (1) <= 'z')
                           or else (Line (1) = '+')
                           or else (Line (1) = '/'))
               then
                  cb := DeBase64 (Line (1 .. 12));
                  b := Decompress (cb);
                  pragma Assert (Compress (b) = cb);
                  sms := Get_Spot_Move_Score (Line (14 .. Line'Last));
                  if not Game_Book.Contains (b) then
                     Game_Book.Insert (b, sms);
                  else
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "Duplicate entry in book: "
                        & Compress_Base64 (cb)
                        & " "
                        & Line (14 .. Line'Last));
                  end if;
               end if;
            end;
         end loop;
         Ada.Text_IO.Close (File);

      end;
      return;
   end Load_Book;

   function Is_Book_Move (cb : Compressed_Board) return Boolean is
      pragma Obsolescent (Is_Book_Move, "No need to compress boards");
   begin
      return Game_Book.Contains (Decompress (cb));
   end Is_Book_Move;

   function Is_Book_Move (b : Game_State) return Boolean is
   begin
      return Game_Book.Contains (b);
   end Is_Book_Move;
   pragma Inline (Is_Book_Move);

   function Is_Missing_Move (cb : Compressed_Board) return Boolean is
      pragma Obsolescent (Is_Missing_Move, "No need to compress boards");
   begin
      return Unknown_Move_Book.Contains (Decompress (cb));
   end Is_Missing_Move;

   function Is_Missing_Move (b : Game_State) return Boolean is
   begin
      return Unknown_Move_Book.Contains (b);
   end Is_Missing_Move;
   pragma Inline (Is_Missing_Move);

   function Get_Score (cb : Compressed_Board) return Score is
      pragma Obsolescent ("No need to compress boards");
   begin
      return Game_Book.Element (Decompress (cb)).est_score;
   end Get_Score;

   function Get_Score (b : Game_State) return Score is
   begin
      return Game_Book.Element (b).est_score;
   end Get_Score;
   pragma Inline (Get_Score);

   function Get_Move (cb : Compressed_Board) return Spot_Move_Score is
      pragma Obsolescent ("No need to compress boards");
   begin
      return Game_Book.Element (Decompress (cb));
   end Get_Move;

   function Get_Move (b : Game_State) return Spot_Move_Score is
   begin
      return Game_Book.Element (b);
   end Get_Move;

   procedure Add_Move (b : Game_State) is
      cb  : constant Compressed_Board := Compress (b);
      sms : Spot_Move_Score;
   begin
      if Move_Book.Is_Book_Move (b) then
         return;
      end if;

      sms := Exact_AB.Best_Move (b);
      if sms.exact then
         Game_Book.Insert (b, sms);
         pragma Assert (if b.store (1) = 36 then sms.est_score /= -127);
      else
         Unknown_Move_Book.Insert (b);
         Ada.Text_IO.Put_Line
           ("*** Didn't conclude -"
            & Compress_Base64 (cb)
            & " "
            & To_String (b));
      end if;
   end Add_Move;

   procedure Dump_Move_Book (f : Ada.Text_IO.File_Type) is
      board_found : Boolean;
   begin
      for pieces_on_board in 2 .. 72 loop
         for player1_store in Piece_Count'(0) .. 36 loop
            board_found := False;
            for b_sms in Game_Book.Iterate loop
               declare
                  b            : Game_State := Move_Hash_Map.Key (b_sms);
                  board_pieces : Integer :=
                    72 - Integer (b.store (1)) - Integer (b.store (2));
                  sms           : Spot_Move_Score :=
                    Move_Hash_Map.Element (b_sms);
                  cb           : Compressed_Board := Compress (b);
               begin
                  if board_pieces = pieces_on_board then
                     if player1_store = b.store (1) then
                        if not board_found then
                           board_found := True;
                           Ada.Text_IO.Put_Line ("");
                           Ada.Text_IO.Put_Line
                             ("== "
                              & pieces_on_board'Image
                              & " pieces on the board and "
                              & b.store (1)'Image
                              & " vs "
                              & b.store (2)'Image
                              & " ==");
                        end if;
                        if sms.est_score = 127 then
                           Ada.Text_IO.Put_Line
                             (f,
                              Compress_Base64 (cb)
                              & " 1 "
                              & Board_Spot'Image (sms.move)
                              & " "
                              & To_String (b));
                        elsif sms.est_score = -127 then
                           Ada.Text_IO.Put_Line
                             (f,
                              Compress_Base64 (cb)
                              & " 2 "
                              & Board_Spot'Image (sms.move)
                              & " "
                              & To_String (b));
                        elsif sms.est_score = 0 then
                           Ada.Text_IO.Put_Line
                             (f,
                              Compress_Base64 (cb)
                              & " 0 "
                              & Board_Spot'Image (sms.move)
                              & " "
                              & To_String (b));
                        else
                           raise Constraint_Error;
                        end if;
                     end if;
                  end if;
               end;
            end loop;
         end loop;
      end loop;
   end Dump_Move_Book;

end Move_Book;
