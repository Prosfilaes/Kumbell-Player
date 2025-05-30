package body Player is

   function Next (p : Player_Type) return Player_Type is
   begin
      case p is
         when 1 =>
            return 2;

         when 2 =>
            return 1;
      end case;
   end Next;

   function To_String (p : Player_Type) return String is
   begin
      return "Player " & p'Image;
   end To_String;
end Player;
