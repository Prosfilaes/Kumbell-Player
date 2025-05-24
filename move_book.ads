with Board; use Board;
with Ada.Text_IO;

package Move_Book is
   procedure Load_Book (filename : String);
   function Is_Book_Move (b : Game_State) return Boolean;
   function Is_Missing_Move (b : Game_State) return Boolean;
   function Get_Score (b : Game_State) return Score;
   function Get_Move
     (b: Game_State) return Spot_Move_Score; 
   procedure Add_Move (b : Game_State; depth: Natural)
   with Pre => Is_Legal_Board (b) and then not Game_Over (b);
   procedure Dump_Move_Book (f : Ada.Text_IO.File_Type);
   procedure Add_Missing (depth: Natural);
   procedure Missing_Move_Insert (b : Game_State);

end move_book;
