% Facts - connects
connects(a,d).
connects(b,c).
connects(c,d).
connects(d,e).

% Facts - floors
floors(a,[a1,a2]).
floors(b,[b1]).
floors(c,[c1,c2,c3]).
floors(d,[d1]).
floors(e,[e2,e3]).

% Facts - elevator
elevator(a,[a1,a2],cel(3,3)).
elevator(b,[b1],cel(2,6)).
elevator(c,[c2,c3],cel(6,3)).
elevator(e,[e2,e3],cel(7,6)).

% Facts - passage
passage(a,d,a1,d1,cel(2,4),cel(1,2)).
passage(b,c,b1,c3,cel(10,10),cel(7,6)).
passage(c,d,c2,d1,cel(1,3),cel(5,2)).
passage(d,e,d1,e3,cel(4,5),cel(1,6)).
