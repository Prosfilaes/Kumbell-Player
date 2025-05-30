with Board; use Board;
with Player; use Player;
with Ada.Text_IO;

package Move_Book is

   type Move_Score_Type is record
      Move : Move_Type;
      Score : Winner_Type;
      Exact : Boolean;
   end record;
   procedure Load_Book (filename : String);
   function Is_Book_Move (b : Game_State_Type) return Boolean
   with Inline;
   function Get_Score (b : Game_State_Type) return Winner_Type
   with Inline;
   function Get_Move (b : Game_State_Type) return Move_Score_Type
   with Inline;
   procedure Add_Move (b : Game_State_Type; depth : Natural)
   with Pre => Is_Legal_Board (b) and then not Game_Over (b);
   procedure Dump_Move_Book (f : Ada.Text_IO.File_Type);
   procedure Add_Missing (depth : Natural);
   procedure Missing_Move_Insert (b : Game_State_Type);

end move_book;
