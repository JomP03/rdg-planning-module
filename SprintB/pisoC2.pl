assert_c2 :-
    assert(m(c,c2,1,1,1)),
    assert(m(c,c2,2,1,1)),
    assert(m(c,c2,3,1,1)),
    assert(m(c,c2,4,1,1)),
    assert(m(c,c2,5,1,1)),
    assert(m(c,c2,6,1,1)),
    assert(m(c,c2,7,1,1)),

    assert(m(c,c2,1,2,0)),
    assert(m(c,c2,2,2,0)),
    assert(m(c,c2,3,2,0)),
    assert(m(c,c2,4,2,0)),
    assert(m(c,c2,5,2,0)),
    assert(m(c,c2,6,2,0)),
    assert(m(c,c2,7,2,1)),

    assert(m(c,c2,1,3,0)),
    assert(m(c,c2,2,3,0)),
    assert(m(c,c2,3,3,0)),
    assert(m(c,c2,4,3,0)),
    assert(m(c,c2,5,3,0)),
    assert(m(c,c2,6,3,0)),
    assert(m(c,c2,7,3,1)),

    assert(m(c,c2,1,4,1)),
    assert(m(c,c2,2,4,1)),
    assert(m(c,c2,3,4,1)),
    assert(m(c,c2,4,4,0)),
    assert(m(c,c2,5,4,1)),
    assert(m(c,c2,6,4,1)),
    assert(m(c,c2,7,4,1)),

    assert(m(c,c2,1,5,1)),
    assert(m(c,c2,2,5,1)),
    assert(m(c,c2,3,5,1)),
    assert(m(c,c2,4,5,1)),
    assert(m(c,c2,5,5,1)),
    assert(m(c,c2,6,5,1)),
    assert(m(c,c2,7,5,1)),

    assert(m(c,c2,1,6,1)),
    assert(m(c,c2,2,6,1)),
    assert(m(c,c2,3,6,1)),
    assert(m(c,c2,4,6,1)),
    assert(m(c,c2,5,6,1)),
    assert(m(c,c2,6,6,1)),
    assert(m(c,c2,7,6,1)),

    assert(m(c,c2,1,7,1)),
    assert(m(c,c2,2,7,1)),
    assert(m(c,c2,3,7,1)),
    assert(m(c,c2,4,7,1)),
    assert(m(c,c2,5,7,1)),
    assert(m(c,c2,6,7,1)),
    assert(m(c,c2,7,7,1)).

load_c2 :- 
    assert_c2,
    build_graph(7,7),
    % Clear dynamic m
    retractall(m(_,_,_,_,_)).