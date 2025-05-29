pragma Restrictions(No_Obsolescent_Features);

with Board; use Board;
with Alpha_Beta;
with Ada.Text_IO;

procedure Main is
begin
    for depth in 1 .. 40 loop
    declare
        b : constant Game_State := Initialize;
        move : constant Alpha_Beta.Spot_Move_Score := Alpha_Beta.Best_Move(b, depth);
    begin
        Ada.Text_IO.Put ("Depth ");
        Ada.Text_IO.Put (depth'Image);
        Ada.Text_IO.Put (": best score ");
        Ada.Text_IO.Put_Line (move'Image);
    end;
end loop;
end Main;


