assert_c3 :-
    assert(m(c,c3,1,1,1)),
    assert(m(c,c3,2,1,1)),
    assert(m(c,c3,3,1,1)),
    assert(m(c,c3,4,1,1)),
    assert(m(c,c3,5,1,1)),
    assert(m(c,c3,6,1,1)),
    assert(m(c,c3,7,1,1)),

    assert(m(c,c3,1,2,1)),
    assert(m(c,c3,2,2,0)),
    assert(m(c,c3,3,2,0)),
    assert(m(c,c3,4,2,0)),
    assert(m(c,c3,5,2,0)),
    assert(m(c,c3,6,2,0)),
    assert(m(c,c3,7,2,1)),

    assert(m(c,c3,1,3,1)),
    assert(m(c,c3,2,3,0)),
    assert(m(c,c3,3,3,0)),
    assert(m(c,c3,4,3,0)),
    assert(m(c,c3,5,3,0)),
    assert(m(c,c3,6,3,0)),
    assert(m(c,c3,7,3,1)),

    assert(m(c,c3,1,4,1)),
    assert(m(c,c3,2,4,1)),
    assert(m(c,c3,3,4,1)),
    assert(m(c,c3,4,4,0)),
    assert(m(c,c3,5,4,0)),
    assert(m(c,c3,6,4,1)),
    assert(m(c,c3,7,4,1)),

    assert(m(c,c3,1,5,1)),
    assert(m(c,c3,2,5,1)),
    assert(m(c,c3,3,5,1)),
    assert(m(c,c3,4,5,0)),
    assert(m(c,c3,5,5,0)),
    assert(m(c,c3,6,5,0)),
    assert(m(c,c3,7,5,1)),

    assert(m(c,c3,1,6,1)),
    assert(m(c,c3,2,6,1)),
    assert(m(c,c3,3,6,0)),
    assert(m(c,c3,4,6,0)),
    assert(m(c,c3,5,6,0)),
    assert(m(c,c3,6,6,0)),
    assert(m(c,c3,7,6,0)),

    assert(m(c,c3,1,7,1)),
    assert(m(c,c3,2,7,1)),
    assert(m(c,c3,3,7,1)),
    assert(m(c,c3,4,7,1)),
    assert(m(c,c3,5,7,1)),
    assert(m(c,c3,6,7,1)),
    assert(m(c,c3,7,7,0)).

load_c3 :- 
    assert_c3,
    build_graph(7,7),
    % Clear dynamic m
    retractall(m(_,_,_,_,_)).