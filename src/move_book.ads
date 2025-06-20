with Board;  use Board;
with Player; use Player;
with Ada.Text_IO;

package Move_Book is

   type Option_Winner_Type is record
      Found  : Boolean;
      Winner : Winner_Type;
   end record;

   procedure Load_Book
     (infilename      : String;
      outfilename     : String := "";
      outworkfilename : String := "");
   function Get_Score (b : Compressed_Board) return Option_Winner_Type
   with Inline;
   procedure Add_Move (b : Game_State_Type; depth : Natural)
   with Pre => Is_Legal_Board (b) and then not Game_Over (b);
   procedure Missing_Move_Insert (b : Compressed_Board);
   procedure Close;

end move_book;
