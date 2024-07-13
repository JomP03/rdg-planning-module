assert_r1 :-
    assert(m(r,r1,1,1,0)),
    assert(m(r,r1,2,1,0)),
    assert(m(r,r1,3,1,0)),
    assert(m(r,r1,4,1,0)),
    assert(m(r,r1,5,1,0)),

    assert(m(r,r1,1,2,0)),
    assert(m(r,r1,2,2,1)),
    assert(m(r,r1,3,2,1)),
    assert(m(r,r1,4,2,0)),
    assert(m(r,r1,5,2,0)),

    assert(m(r,r1,1,3,0)),
    assert(m(r,r1,2,3,1)),
    assert(m(r,r1,3,3,1)),
    assert(m(r,r1,4,3,0)),
    assert(m(r,r1,5,3,0)),

    assert(m(r,r1,1,4,0)),
    assert(m(r,r1,2,4,0)),
    assert(m(r,r1,3,4,0)),
    assert(m(r,r1,4,4,0)),
    assert(m(r,r1,5,4,0)).

load_r1 :- 
    assert_r1,
    build_graph(5,4),
    % Clear dynamic m
    retractall(m(_,_,_,_,_)).