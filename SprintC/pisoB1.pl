assert_b1 :-
    assert(m(b,b1,1,1,0)),
    assert(m(b,b1,2,1,0)),
    assert(m(b,b1,3,1,0)),
    assert(m(b,b1,4,1,0)),
    assert(m(b,b1,5,1,0)),
    assert(m(b,b1,6,1,0)),

    assert(m(b,b1,1,2,0)),
    assert(m(b,b1,2,2,1)),
    assert(m(b,b1,3,2,1)),
    assert(m(b,b1,4,2,0)),
    assert(m(b,b1,5,2,0)),
    assert(m(b,b1,6,2,1)),

    assert(m(b,b1,1,3,0)),
    assert(m(b,b1,2,3,1)),
    assert(m(b,b1,3,3,1)),
    assert(m(b,b1,4,3,0)),
    assert(m(b,b1,5,3,0)),
    assert(m(b,b1,6,3,0)),

    assert(m(b,b1,1,4,0)),
    assert(m(b,b1,2,4,1)),
    assert(m(b,b1,3,4,1)),
    assert(m(b,b1,4,4,0)),
    assert(m(b,b1,5,4,0)),
    assert(m(b,b1,6,4,0)),

    assert(m(b,b1,1,5,0)),
    assert(m(b,b1,2,5,0)),
    assert(m(b,b1,3,5,0)),
    assert(m(b,b1,4,5,0)),
    assert(m(b,b1,5,5,0)),
    assert(m(b,b1,6,5,0)).

load_b1 :- 
    assert_b1,
    build_graph(6,5),
    % Clear dynamic m
    retractall(m(_,_,_,_,_)).