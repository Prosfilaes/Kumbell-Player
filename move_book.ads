with Board; use Board;
with Player;

package Move_Book is
   function Load_Book (filename : String) return Boolean;
   function Is_Book_Move (cb : Compressed_Board) return Boolean;
   function Get_Score (cb : Compressed_Board) return Score;
   function Get_Move (cb : Compressed_Board; p: Player.Player) return Spot_Move_Score;
end move_book;