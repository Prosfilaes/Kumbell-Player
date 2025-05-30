pragma Restrictions (No_Obsolescent_Features);

with Ada.Text_IO;
with Board;
with Move_Book;

procedure Build_From_Start is
   b: constant Board.Game_State_Type := Board.Initialize;
begin
   Move_Book.Load_Book ("move_book.table");
   Ada.Text_IO.Put_Line ("* Loaded book and starting " & Board.To_String(b));
   Move_Book.Add_Move (b, 1);
   Move_Book.Add_Missing (1);
   Move_Book.Dump_Move_Book (Ada.Text_IO.Standard_Output);
end Build_From_Start;
