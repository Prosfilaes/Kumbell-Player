package body Player is

    function Next (p : Player) return Player is
    begin
        if p = 1 then
            return 2;
        else
           return 1;
       end if;
    end Next; 

    function To_String (p : Player) return String is
    begin
        return "Player " & p'Image;
    end To_String;
end Player;
