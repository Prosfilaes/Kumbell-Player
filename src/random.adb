with Ada.Numerics.Discrete_Random;

package body Random is

   package Rand_Move is new Ada.Numerics.Discrete_Random (Board_Spot);

   Initalized : Boolean := false;
   Gen        : Rand_Move.Generator;

   function Random_Move (b : Game_State) return Board_Spot is
   begin
      if not Initalized then
         Rand_Move.Reset (Gen);
         Initalized := true;
      end if;
      loop
         declare
            m : constant Board_Spot := Rand_Move.Random (Gen);
         begin
            if Is_Legal_Move (b, m) then
               return m;
            end if;
         end;
      end loop;
   end Random_Move;

end Random;
