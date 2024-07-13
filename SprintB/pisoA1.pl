assert_a1 :-
    assert(m(a,a1,1,1,1)),
    assert(m(a,a1,2,1,1)),
    assert(m(a,a1,3,1,1)),
    assert(m(a,a1,4,1,1)),

    assert(m(a,a1,1,2,1)),
    assert(m(a,a1,2,2,0)),
    assert(m(a,a1,3,2,1)),
    assert(m(a,a1,4,2,1)),

    assert(m(a,a1,1,3,1)),
    assert(m(a,a1,2,3,0)),
    assert(m(a,a1,3,3,0)),
    assert(m(a,a1,4,3,1)),

    assert(m(a,a1,1,4,1)),
    assert(m(a,a1,2,4,0)),
    assert(m(a,a1,3,4,0)),
    assert(m(a,a1,4,4,1)).

load_a1 :- 
    assert_a1,
    build_graph(4,4),
    % Clear dynamic m
    retractall(m(_,_,_,_,_)).