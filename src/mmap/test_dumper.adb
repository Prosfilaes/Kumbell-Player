with Ada.Text_IO;
with Mmap;
with Ada.Command_Line;

procedure Test_Dumper is
begin
   Mmap.Open (Ada.Command_Line.Argument(1));

   for I in 0 .. Mmap.Length - 1 loop
      declare
           R : Mmap.Mmap_Record := Mmap.Get (I);
      begin
      Ada.Text_IO.Put_Line ("Start: " & R.Start_Idx'Image &
                " End: " & R.End_Idx'Image &
                " Value: " & R.Value'Image);
   end;
   end loop;

   Mmap.Close;
end Test_Dumper;
