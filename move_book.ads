with Board; use Board;
with Ada.Text_IO;
with Player;

package Move_Book is
   procedure Load_Book (filename : String);
   function Is_Book_Move (cb : Compressed_Board) return Boolean;
   function Get_Score (cb : Compressed_Board) return Score;
   function Get_Move
     (cb : Compressed_Board; p : Player.Player) return Spot_Move_Score;
   procedure Add_Move (f : Ada.Text_IO.File_Type; b : Game_State)
   with Pre => Is_Legal_Board (b) and then not Game_Over (b);

end move_book;
