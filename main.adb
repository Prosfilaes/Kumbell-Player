pragma Restrictions(No_Obsolescent_Features);

with Board; use Board;
with Alpha_Beta;
with Ada.Text_IO;

procedure Main is
begin
    for depth in 1 .. 12 loop
    declare
        b : constant Game_State := Initialize;
        move : constant Alpha_Beta.Score := Alpha_Beta.Best_Move(b, depth, b.curr_player);
    begin
        Ada.Text_IO.Put ("Depth ");
        Ada.Text_IO.Put (depth'Image);
        Ada.Text_IO.Put (": first move ");
        Ada.Text_IO.Put_Line (move'Image);
    end;
end loop;
end Main;


