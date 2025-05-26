pragma Restrictions (No_Obsolescent_Features);

with Board;
with Move_Book;

procedure Build_From_Start is
   b: Board.Game_State;
begin
   Move_Book.Load_Book ("move_book.table");
   b := Board.Initialize;
   b.store(1) := 24;
   b.store(2) := 24;
   for i in Board.Board_Spot(1) .. 12 loop
      b.board(i) := 2;
   end loop;
   Move_Book.Add_Move (b, 10);
   Move_Book.Add_Missing (10);
end Build_From_Start;
