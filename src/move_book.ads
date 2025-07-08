with Board;  use Board;
with Player; use Player;
with Uniq_Max_Heap;

package Move_Book is

   type Option_Winner_Type is record
      Found  : Boolean;
      Winner : Winner_Type;
   end record;

   procedure Load_Book
     (infilename      : String := "";
      outfilename     : String := "";
      outworkfilename : String := "");
   function Get_Score (b : Compressed_Board) return Option_Winner_Type
   with Inline;
   procedure Add_Move (b : Game_State_Type)
   with Pre => Is_Legal_Board (b) and then not Game_Over (b);
   procedure Missing_Move_Insert (b : Compressed_Board);
   procedure Close;

   package Move_Heap_P is new Uniq_Max_Heap (Compressed_Board);
   function Get_Missing_Move_Heap return Move_Heap_P.Max_Heap_Type;
   procedure Reset_Missing_Move_Heap;
   procedure Set_Max_Heap_Size (size : Natural);
end move_book;
