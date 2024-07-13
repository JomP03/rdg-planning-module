assert_d1 :-
    assert(m(d,d1,1,1,1)),
    assert(m(d,d1,2,1,1)),
    assert(m(d,d1,3,1,1)),
    assert(m(d,d1,4,1,1)),
    assert(m(d,d1,5,1,1)),

    assert(m(d,d1,1,2,0)),
    assert(m(d,d1,2,2,0)),
    assert(m(d,d1,3,2,0)),
    assert(m(d,d1,4,2,0)),
    assert(m(d,d1,5,2,0)),

    assert(m(d,d1,1,3,0)),
    assert(m(d,d1,2,3,0)),
    assert(m(d,d1,3,3,0)),
    assert(m(d,d1,4,3,0)),
    assert(m(d,d1,5,3,0)),

    assert(m(d,d1,1,4,1)),
    assert(m(d,d1,2,4,1)),
    assert(m(d,d1,3,4,0)),
    assert(m(d,d1,4,4,0)),
    assert(m(d,d1,5,4,1)),

    assert(m(d,d1,1,5,1)),
    assert(m(d,d1,2,5,1)),
    assert(m(d,d1,3,5,1)),
    assert(m(d,d1,4,5,0)),
    assert(m(d,d1,5,5,0)).

load_d1 :- 
    assert_d1,
    build_graph(5,5),
    % Clear dynamic m
    retractall(m(_,_,_,_,_)).