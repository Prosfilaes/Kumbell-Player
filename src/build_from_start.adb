pragma Restrictions (No_Obsolescent_Features);

with Ada.Text_IO;
with Board;
with Move_Book;

procedure Build_From_Start is
   b: Board.Game_State;
begin
   Move_Book.Load_Book ("move_book.table");
   b := Board.Initialize;
--   b.store(1) := 24;
--   b.store(2) := 24;
--   for i in Board.Board_Spot(1) .. 12 loop
--      b.board(i) := 2;
--   end loop;
   Ada.Text_IO.Put_Line ("* Loaded book and starting " & Board.To_String(b));
   Move_Book.Add_Move (b, 4);
   Move_Book.Add_Missing (4);
   Move_Book.Dump_Move_Book (Ada.Text_IO.Standard_Output);
end Build_From_Start;
