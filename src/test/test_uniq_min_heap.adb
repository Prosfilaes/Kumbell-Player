with Uniq_Min_Heap;
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Containers.Ordered_Sets;

procedure Test_Uniq_Min_Heap is
   package Int_Rand is new Ada.Numerics.Discrete_Random (Short_Integer);
   package Int_UMH is new Uniq_Min_Heap (Short_Integer);
   use Int_UMH;
   package Int_Set is new Ada.Containers.Ordered_Sets (Short_Integer);
   use Int_Set;
   gen              : Int_Rand.Generator;
   s                : Int_Set.Set;
   umh              : Int_UMH.Min_Heap_Type;
   last_num_started : Boolean := False;
   last_num         : Short_Integer;
   new_num          : Short_Integer;
begin
   for i in 1 .. 1_000_000 loop
      new_num := Int_Rand.Random (gen);
      -- Why do I have to double check this? It's slower than
      -- just adding it if it's not already there.
      if not Contains (s, new_num) then
         Insert (s, new_num);
      end if;
      Insert (umh, new_num);

      if not Verify_Heap (umh) then
         Ada.Text_IO.Put_Line (new_num'Image);
         Ada.Text_IO.Put_Line (Debug_Image (umh));
         Ada.Text_IO.Put_Line ("Verify fail.");
      end if;
      if i mod 500000 = 0 then
         Ada.Text_IO.Put_Line ("Compacting...");
         Compact (umh);
      end if;
      if i mod 100 = 0 then
         Ada.Text_IO.Put_Line
           ("Count: "
            & i'Image
            & " Set size: "
            & s.Length'Image
            & " Min heap size: "
            & umh.Size'Image);
      end if;
   end loop;
   while (Size (umh) > 0) loop
      Pop_Min (umh, new_num);
      if (not Contains (s, new_num)) then
         Ada.Text_IO.Put_Line
           ("Set size: "
            & s.Length'Image
            & " Min heap size: "
            & umh.Size'Image);
         Ada.Text_IO.Put_Line
           ("Removed " & new_num'Image & " from umh, but not found in set");
      else
         Delete (s, new_num);
      end if;
      if last_num_started and last_num >= new_num then
         Ada.Text_IO.Put_Line
           ("Out of order removal: last_num: "
            & last_num'Image
            & " came out before new_num "
            & new_num'Image);
      end if;
      last_num_started := True;
      last_num := new_num;
   end loop;
   Ada.Text_IO.Put_Line
     ("UMH should be empty and set should be empty. Sizes are "
      & umh.Size'Image
      & " "
      & s.Length'Image);
end Test_Uniq_Min_Heap;
