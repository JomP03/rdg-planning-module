assert_b2 :-
    assert(m(b,b2,1,1,0)),
    assert(m(b,b2,2,1,0)),
    assert(m(b,b2,3,1,0)),
    assert(m(b,b2,4,1,0)),
    assert(m(b,b2,5,1,0)),
    assert(m(b,b2,6,1,0)),

    assert(m(b,b2,1,2,0)),
    assert(m(b,b2,2,2,1)),
    assert(m(b,b2,3,2,1)),
    assert(m(b,b2,4,2,0)),
    assert(m(b,b2,5,2,0)),
    assert(m(b,b2,6,2,1)),

    assert(m(b,b2,1,3,0)),
    assert(m(b,b2,2,3,1)),
    assert(m(b,b2,3,3,1)),
    assert(m(b,b2,4,3,0)),
    assert(m(b,b2,5,3,0)),
    assert(m(b,b2,6,3,0)),

    assert(m(b,b2,1,4,0)),
    assert(m(b,b2,2,4,1)),
    assert(m(b,b2,3,4,1)),
    assert(m(b,b2,4,4,0)),
    assert(m(b,b2,5,4,0)),
    assert(m(b,b2,6,4,0)),

    assert(m(b,b2,1,5,0)),
    assert(m(b,b2,2,5,0)),
    assert(m(b,b2,3,5,0)),
    assert(m(b,b2,4,5,0)),
    assert(m(b,b2,5,5,0)),
    assert(m(b,b2,6,5,0)).

load_b2 :- 
    assert_b2,
    build_graph(6,5),
    % Clear dynamic m
    retractall(m(_,_,_,_,_)).