assert_c2 :-
    assert(m(c,c2,1,1,0)),
    assert(m(c,c2,2,1,0)),
    assert(m(c,c2,3,1,0)),
    assert(m(c,c2,4,1,0)),
    assert(m(c,c2,5,1,0)),
    assert(m(c,c2,6,1,0)),
    assert(m(c,c2,7,1,0)),
    assert(m(c,c2,8,1,0)),
    assert(m(c,c2,9,1,0)),

    assert(m(c,c2,1,2,0)),
    assert(m(c,c2,2,2,0)),
    assert(m(c,c2,3,2,0)),
    assert(m(c,c2,4,2,0)),
    assert(m(c,c2,5,2,0)),
    assert(m(c,c2,6,2,0)),
    assert(m(c,c2,7,2,0)),
    assert(m(c,c2,8,2,0)),
    assert(m(c,c2,9,2,0)),

    assert(m(c,c2,1,3,0)),
    assert(m(c,c2,2,3,0)),
    assert(m(c,c2,3,3,1)),
    assert(m(c,c2,4,3,1)),
    assert(m(c,c2,5,3,1)),
    assert(m(c,c2,6,3,1)),
    assert(m(c,c2,7,3,0)),
    assert(m(c,c2,8,3,0)),
    assert(m(c,c2,9,3,0)),

    assert(m(c,c2,1,4,0)),
    assert(m(c,c2,2,4,0)),
    assert(m(c,c2,3,4,1)),
    assert(m(c,c2,4,4,1)),
    assert(m(c,c2,5,4,1)),
    assert(m(c,c2,6,4,1)),
    assert(m(c,c2,7,4,0)),
    assert(m(c,c2,8,4,1)),
    assert(m(c,c2,9,4,0)),

    assert(m(c,c2,1,5,0)),
    assert(m(c,c2,2,5,0)),
    assert(m(c,c2,3,5,1)),
    assert(m(c,c2,4,5,1)),
    assert(m(c,c2,5,5,1)),
    assert(m(c,c2,6,5,1)),
    assert(m(c,c2,7,5,0)),
    assert(m(c,c2,8,5,0)),
    assert(m(c,c2,9,5,0)),

    assert(m(c,c2,1,6,0)),
    assert(m(c,c2,2,6,0)),
    assert(m(c,c2,3,6,0)),
    assert(m(c,c2,4,6,0)),
    assert(m(c,c2,5,6,0)),
    assert(m(c,c2,6,6,0)),
    assert(m(c,c2,7,6,0)),
    assert(m(c,c2,8,6,0)),
    assert(m(c,c2,9,6,0)).

load_c2 :- 
    assert_c2,
    build_graph(9,6),
    % Clear dynamic m
    retractall(m(_,_,_,_,_)).