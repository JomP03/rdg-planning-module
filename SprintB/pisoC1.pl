assert_c1 :-
    assert(m(c,c1,1,1,1)),
    assert(m(c,c1,2,1,1)),
    assert(m(c,c1,3,1,1)),
    assert(m(c,c1,4,1,1)),
    assert(m(c,c1,5,1,1)),
    assert(m(c,c1,6,1,1)),
    assert(m(c,c1,7,1,1)),

    assert(m(c,c1,1,2,1)),
    assert(m(c,c1,2,2,0)),
    assert(m(c,c1,3,2,0)),
    assert(m(c,c1,4,2,0)),
    assert(m(c,c1,5,2,0)),
    assert(m(c,c1,6,2,0)),
    assert(m(c,c1,7,2,1)),

    assert(m(c,c1,1,3,1)),
    assert(m(c,c1,2,3,0)),
    assert(m(c,c1,3,3,0)),
    assert(m(c,c1,4,3,0)),
    assert(m(c,c1,5,3,0)),
    assert(m(c,c1,6,3,0)),
    assert(m(c,c1,7,3,1)),

    assert(m(c,c1,1,4,1)),
    assert(m(c,c1,2,4,1)),
    assert(m(c,c1,3,4,1)),
    assert(m(c,c1,4,4,0)),
    assert(m(c,c1,5,4,1)),
    assert(m(c,c1,6,4,1)),
    assert(m(c,c1,7,4,1)),

    assert(m(c,c1,1,5,1)),
    assert(m(c,c1,2,5,1)),
    assert(m(c,c1,3,5,1)),
    assert(m(c,c1,4,5,1)),
    assert(m(c,c1,5,5,1)),
    assert(m(c,c1,6,5,1)),
    assert(m(c,c1,7,5,1)),

    assert(m(c,c1,1,6,1)),
    assert(m(c,c1,2,6,1)),
    assert(m(c,c1,3,6,1)),
    assert(m(c,c1,4,6,1)),
    assert(m(c,c1,5,6,1)),
    assert(m(c,c1,6,6,1)),
    assert(m(c,c1,7,6,1)),

    assert(m(c,c1,1,7,1)),
    assert(m(c,c1,2,7,1)),
    assert(m(c,c1,3,7,1)),
    assert(m(c,c1,4,7,1)),
    assert(m(c,c1,5,7,1)),
    assert(m(c,c1,6,7,1)),
    assert(m(c,c1,7,7,1)).

load_c1 :-
    assert_c1,
    build_graph(7,7),
    % Clear dynamic m
    retractall(m(_,_,_,_,_)).