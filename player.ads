package Player is
    
    type Player is range 1 .. 2;

    function next (p: Player) return Player;
    function to_string (p : Player) return String;
end Player;
