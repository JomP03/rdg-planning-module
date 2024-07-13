assert_a2 :- 
    assert(m(a,a2,1,1,1)),
    assert(m(a,a2,2,1,1)),
    assert(m(a,a2,3,1,1)),
    assert(m(a,a2,4,1,1)),

    assert(m(a,a2,1,2,1)),
    assert(m(a,a2,2,2,0)),
    assert(m(a,a2,3,2,1)),
    assert(m(a,a2,4,2,1)),

    assert(m(a,a2,1,3,1)),
    assert(m(a,a2,2,3,0)),
    assert(m(a,a2,3,3,0)),
    assert(m(a,a2,4,3,1)),

    assert(m(a,a2,1,4,1)),
    assert(m(a,a2,2,4,1)),
    assert(m(a,a2,3,4,1)),
    assert(m(a,a2,4,4,1)).

load_a2 :- 
    assert_a2,
    build_graph(4,4),
    % Clear dynamic m
    retractall(m(_,_,_,_,_)).