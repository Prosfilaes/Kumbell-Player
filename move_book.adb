with Ada.Integer_Text_IO;
with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Player; use Player;
with Exact_AB;
with Alpha_Beta;

package body Move_Book is
   function Hash (board : Compressed_Board) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (board mod 2**32 xor (board / 2**32));
   end Hash;

   function Is_Equal (a, b : Compressed_Board) return Boolean is
   begin
      return a = b;
   end Is_Equal;

   package Move_Hash_Map is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => Compressed_Board,
        Element_Type    => Spot_Move_Score,
        Hash            => Hash,
        Equivalent_Keys => Is_Equal);

   Game_Book : Move_Hash_Map.Map;

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
         Cb   : Compressed_Board;
         S    : Spot_Move_Score;
      begin
         Ada.Text_IO.Open (File, Ada.Text_IO.In_File, filename);
         while not Ada.Text_IO.End_Of_File (File) loop
            declare
               Line : constant string := Ada.Text_IO.Get_Line (File);
            begin
               -- Ignore all lines that don't fit the format
               if Line'Length >= 14
                 and then ((Line (1) >= '0' and then Line (1) <= '9')
                           or else (Line (1) >= 'A' and then Line (1) <= 'Z')
                           or else (Line (1) >= 'a' and then Line (1) <= 'z')
                           or else (Line (1) = '+')
                           or else (Line (1) = '/'))
               then
                  Cb := DeBase64 (Line (1 .. 12));
                  S := Get_Spot_Move_Score (Line (14 .. Line'Last));
                  if not Game_Book.Contains (Cb) then
                     Game_Book.Insert (Cb, S);
                  else
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "Duplicate entry in book: "
                        & Compress_Base64 (Cb)
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
   begin
      return Game_Book.Contains (cb);
   end Is_Book_Move;

   function Get_Score (cb : Compressed_Board) return Score is
   begin
      return Game_Book.Element (cb).est_score;
   end Get_Score;

   function Get_Move
     (cb : Compressed_Board; p : Player.Player) return Spot_Move_Score
   is
      sms : Spot_Move_Score := Game_Book.Element (cb);
   begin
      if p = 2 then
         -- Swap the move for player 2
         sms.move := Board_Spot (6 + sms.move);
      end if;
      return sms;
   end Get_Move;

   procedure Add_Move (f : Ada.Text_IO.File_Type; b : Game_State) is
      cb  : constant Compressed_Board := Compress (b);
      sms : Spot_Move_Score;
   begin
      if Move_Book.Is_Book_Move (cb) then
         return;
      end if;

      begin
         sms := Exact_AB.Best_Move (b);

         Game_Book.Insert (cb, sms);
         if sms.est_score = 127 then
            Ada.Text_IO.Put_Line
              (f,
               Compress_Base64 (cb)
               & " 1 "
               & Board_Spot'Image (sms.move));
         elsif sms.est_score = -127 then
            Ada.Text_IO.Put_Line
              (f,
               Compress_Base64 (cb)
               & " 2 "
               & Board_Spot'Image (sms.move));
         elsif sms.est_score = 0 then
            Ada.Text_IO.Put_Line
              (f,
               Compress_Base64 (cb)
               & " 0 "
               & Board_Spot'Image (sms.move));
         else
            raise Constraint_Error;
         end if;
      exception
         when Exact_AB.Stack_Overflow_Error =>
            Ada.Text_IO.Put_Line
              (f,
               "*** Didn't conclude -"
               & Compress_Base64 (cb)
               & " "
               & sms.est_score'Image 
               & " " 
               & To_String (b));
      end;
   end Add_Move;

   procedure Add_Move
     (f : Ada.Text_IO.File_Type; b : Game_State; depth : Integer)
   is
      cb  : constant Compressed_Board := Compress (b);
      sms : Spot_Move_Score;
   begin
      if Move_Book.Is_Book_Move (cb) then
         return;
      -- sms := Move_Book.Get_Move (cb, b.curr_player);

      else
         sms := Alpha_Beta.Best_Move (b, depth, True);
      end if;
      if sms.exact then
         Game_Book.Insert (cb, sms);
         if sms.est_score = 127 then
            Ada.Text_IO.Put_Line
              (f,
               Compress_Base64 (Compress (b))
               & " 1 "
               & Board_Spot'Image (sms.move));
         elsif sms.est_score = -127 then
            Ada.Text_IO.Put_Line
              (f,
               Compress_Base64 (Compress (b))
               & " 2 "
               & Board_Spot'Image (sms.move));
         elsif sms.est_score = 0 then
            Ada.Text_IO.Put_Line
              (f,
               Compress_Base64 (Compress (b))
               & " 0 "
               & Board_Spot'Image (sms.move));
         else
            raise Constraint_Error;
         end if;
      else
         Ada.Text_IO.Put_Line
           (f,
            "*** Didn't conclude -"
            & Compress_Base64 (Compress (b))
            & " "
            & sms.est_score'Image);
      end if;
   end Add_Move;

end Move_Book;
