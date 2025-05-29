with Board; use Board;
with Ada.Text_IO;

package Move_Book is
   procedure Load_Book (filename : String);
   function Is_Book_Move (b : Game_State) return Boolean
   with Inline;
   function Get_Score (b : Game_State) return Score
   with Inline;
   function Get_Move (b : Game_State) return Spot_Move_Score
   with Inline;
   procedure Add_Move (b : Game_State; depth : Natural)
   with Pre => Is_Legal_Board (b) and then not Game_Over (b);
   procedure Dump_Move_Book (f : Ada.Text_IO.File_Type);
   procedure Add_Missing (depth : Natural);
   procedure Missing_Move_Insert (b : Game_State);

end move_book;
