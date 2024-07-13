% Facts - connects
connects(a,b).
connects(a,c).
connects(b,r).
connects(c,r).

% Facts - floors
floors(a,[a1]).
floors(b,[b1,b2]).
floors(c,[c1,c2]).
floors(r,[r1]).

% Facts - elevator
elevator(b,[b1,b2],cel(6,3)).
elevator(c,[c1,c2],cel(9,4)).

% Facts - passage
passage(a,b,a1,b1,cel(1,1),cel(5,1)).
passage(a,c,a1,c2,cel(4,4),cel(8,6)).
passage(b,r,b1,r1,cel(5,5),cel(1,1)).
passage(c,r,c2,r1,cel(9,1),cel(5,4)).
