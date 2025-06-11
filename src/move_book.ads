with Board; use Board;
with Player; use Player;
with Ada.Text_IO;

package Move_Book is

   type Move_Score_Type is record
      Move : Move_Type;
      Score : Winner_Type;
      Exact : Boolean;
   end record;
   procedure Load_Book (infilename : String; outfilename : String := ""; outworkfilename : String := "");
   function Is_Book_Move (b : Compressed_Board) return Boolean
   with Inline;
   function Get_Score (b : Compressed_Board) return Winner_Type
   with Inline;
   function Get_Move (b : Compressed_Board) return Move_Score_Type
   with Inline;
   procedure Add_Move (b : Game_State_Type; depth : Natural)
   with Pre => Is_Legal_Board (b) and then not Game_Over (b);
   procedure Dump_Move_Book (f : Ada.Text_IO.File_Type);
   procedure Missing_Move_Insert (b : Compressed_Board);
   procedure Close;

end move_book;
