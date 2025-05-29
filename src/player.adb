package body Player is

   function Next (p : Player) return Player is
   begin
      case p is
         when 1 =>
            return 2;

         when 2 =>
            return 1;
      end case;
   end Next;

   function To_String (p : Player) return String is
   begin
      return "Player " & p'Image;
   end To_String;
end Player;
