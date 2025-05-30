package Player is
    
    type Winner_Type is range -1 .. 1;
    type Player_Type is range 1 .. 2;

    function Next (p: Player_Type) return Player_Type;
    function To_String (p : Player_Type) return String;
end Player;
