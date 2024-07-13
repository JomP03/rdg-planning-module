assert_tasks :-
    % T1 - Surveillance
    assert(task(t1,a1,b1,cel(2,2),cel(4,3))),
    % T2 - Pick up and deliver
    assert(task(t2,c2,b1,cel(7,3),cel(4,3))),
    % T3 - Surveillance
    assert(task(t3,b2,a1,cel(6,3),cel(2,4))),

    assert(tasks(3)),

    assert(robot(r1,cel(4,3))).

load_tasks :- 
    assert_tasks.