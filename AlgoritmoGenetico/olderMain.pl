% Dynamic facts
:-dynamic generations/1.
:-dynamic population/1.
:-dynamic prob_crossover/1.
:-dynamic prob_mutation/1.

% task(Id,ProcessTime,DueTime,PenaltyWeight).
task(t1,2,5,1).
task(t2,4,7,6).
task(t3,1,11,2).
task(t4,3,9,3).
task(t5,3,8,2).

% tasks(NTasks).
tasks(5).

% parameters initialization
initialize :- 
    write('Number of new generations: '), read(NG),
    (retract(generations(_));true), asserta(generations(NG)),

    write('Population size: '), read(PS),
    (retract(population(_));true), asserta(population(PS)),

    write('Probability of crossover (%):'), read(P1),
    PC is P1/100,
    (retract(prob_crossover(_));true), asserta(prob_crossover(PC)),

    write('Probability of mutation (%):'), read(P2),
    PM is P2/100,
    (retract(prob_mutation(_));true), asserta(prob_mutation(PM)).

generate :-
    initialize,
    generate_population(Pop),
    write('Pop='), write(Pop), nl,
    evaluate_population(Pop,PopValue),
    write('PopValue='), write(PopValue), nl,
    order_population(PopValue,PopOrd),
    generations(NG),
    generate_generation(0,NG,PopOrd).

% To generate a population of individuals
generate_population(Pop) :-
    population(PopSize),
    tasks(NumT),
    findall(Task,task(Task,_,_,_),TasksList),
    generate_population(PopSize,TasksList,NumT,Pop).

generate_population(0,_,_,[]) :- !.

generate_population(PopSize,TasksList,NumT,[Ind|Rest]) :-
    PopSize1 is PopSize-1,
    generate_population(PopSize1,TasksList,NumT,Rest),
    generate_individual(TasksList,NumT,Ind),
    % To avoid repeated individuals
    not(member(Ind,Rest)).

generate_population(PopSize,TasksList,NumT,L) :-
    generate_population(PopSize,TasksList,NumT,L).

% This will generate an individual, i.e., a sequence of tasks
generate_individual([G],1,[G]) :- !.

generate_individual(TasksList,NumT,[G|Rest]) :-
    NumTemp is NumT + 1,
    % To generate a random number between 1 and NumTemp (random generates between 1 and NumTemp-1)
    random(1,NumTemp,N),
    remove(N,TasksList,G,NewList),
    NumT1 is NumT-1,
    generate_individual(NewList,NumT1,Rest).

% Remove the first occurrence of an element from a list
remove(1,[G|Rest],G,Rest).

% Remove the Nth element of a list
remove(N,[G1|Rest],G,[G1|Rest1]) :- 
    N1 is N-1,
    remove(N1,Rest,G,Rest1).

% Base case for the evaluation of the population
evaluate_population([],[]).

evaluate_population([Ind|Rest],[Ind*V|Rest1]) :- 
    % To evaluate an individual
    evaluate(Ind,V),
    % Recursive call to evaluate the rest of the population
    evaluate_population(Rest,Rest1).

% To evaluate an individual
evaluate(Seq,V) :- evaluate(Seq,0,V).

% Base case for the evaluation of an individual
evaluate([ ],_,0).

% Recursive case for the evaluation of an individual
evaluate([T|Rest],Inst,V) :-
    % To get the task information
    task(T,Dur,Due,Pen),
    % Task completion time
    FinInst is Inst+Dur,
    evaluate(Rest,FinInst,VRest),
    ((FinInst =< Due,!, VT is 0) ; (VT is(FinInst - Due)*Pen)),
    V is VT+VRest.

% Order the population according to the fitness value, ascending order
order_population(PopValue,PopValueOrd) :- bsort(PopValue,PopValueOrd).

% Bubble sort
bsort([X],[X]) :- !.

bsort([X|Xs],Ys) :-
    bsort(Xs,Zs),
    bchange([X|Zs],Ys).

bchange([X],[X]) :- !.

bchange([X*VX,Y*VY|L1],[Y*VY|L2]) :-
    VX>VY,!,
    bchange([X*VX|L1],L2).

bchange([X|L1],[X|L2]) :- bchange(L1,L2).

generate_generation(G,G,Pop) :- !,
    write('Generation '), write(G), write(':'), nl, write(Pop), nl.

generate_generation(N,G,Pop):-
    write('Generation '), write(N), write(':'), nl, write(Pop), nl,
    % Remove the first individual of the population, to a separate list
    [Ind1|_] = Pop,
    % Remove the cost of the first individual
    Ind1 = Seq*_,
    % Generate the new population
    write('Pop before crossover:'), nl, write(Pop), nl,

    crossover(Pop,NPop1),

    write('Pop after crossover:'), nl, write(NPop1), nl,

    mutation(NPop1,NPop),

    write('Pop after mutation:'), nl, write(NPop), nl,

    % Add the first individual to the new population
    append([Seq],NPop,NPop2),
    evaluate_population(NPop2,NPopValue),
    % TODO: Add a selection method that uses tournament selection
    order_population(NPopValue,NPopOrd),
    % Remove the last individual of the population
    append(NPopOrdWithoutLast, [_], NPopOrd),
    N1 is N+1,
    generate_generation(N1,G,NPopOrdWithoutLast).

generate_crossover_points(P1,P2) :- generate_crossover_points1(P1,P2).

generate_crossover_points1(P1,P2) :-
    tasks(N),
    NTemp is N+1,
    random(1,NTemp,P11),
    random(1,NTemp,P21),
    P11\==P21,!,
    ((P11<P21,!,P1=P11,P2=P21);P1=P21,P2=P11).

generate_crossover_points1(P1,P2) :- generate_crossover_points1(P1,P2).

generate_crossover_elements(E1,E2,Len) :- generate_crossover_elements1(E1,E2,Len).

generate_crossover_elements1(E1,E2,Len) :-
    % Increment Len to generate a random number between 1 and Len (random generates between 1 and Len-1)
    LenTemp is Len+1,
    random(1,LenTemp,E11),
    random(1,LenTemp,E21),
    E11\==E21,!,
    ((E11<E21,!,E1=E11,E2=E21);E1=E21,E2=E11).

generate_crossover_elements1(E1,E2,Len) :- generate_crossover_elements1(E1,E2,Len).

% Base case when the length of Indivs is 0 or 1, no work needed
crossover([],[]).

crossover([Ind*_],[Ind]).

crossover([Ind1*_,Ind2*_],NInds) :- 
    % Generate the crossover points. P1 and P2 are the positions of the tasks inside the sequence
    generate_crossover_points(P1,P2),
    prob_crossover(Pcruz),random(0.0,1.0,Pc),
    ((Pc =< Pcruz,!,
        cross(Ind1,Ind2,P1,P2,NInd1),
        cross(Ind2,Ind1,P1,P2,NInd2),
        NInds = [NInd1,NInd2]);
    NInds = [Ind1,Ind2]).

crossover(Indivs,[NInd1,NInd2|Rest1]) :-
    % Get Indivs length
    length(Indivs,Len),
    % Generate the crossover elements (the individuals to cross)
    generate_crossover_elements(E1,E2,Len),
    % Get the individuals to cross
    nth1(E1,Indivs,Ind1*_),
    nth1(E2,Indivs,Ind2*_),
    % Generate the crossover points. P1 and P2 are the positions of the tasks inside the sequence
    generate_crossover_points(P1,P2),
    prob_crossover(Pcruz),random(0.0,1.0,Pc),
    ((Pc =< Pcruz,!,
        cross(Ind1,Ind2,P1,P2,NInd1),
        cross(Ind2,Ind1,P1,P2,NInd2));
    (NInd1=Ind1,NInd2=Ind2)),
    % Remove the crossed individuals from the list
    (E1 > E2 -> nth1(E1,Indivs,_,Indivs1), nth1(E2,Indivs1,_,Indivs2);
                nth1(E2,Indivs,_,Indivs1), nth1(E1,Indivs1,_,Indivs2)),
    % Recursive call
    crossover(Indivs2,Rest1).


% crossover([Ind1*_,Ind2*_|Rest],[NInd1,NInd2|Rest1]) :-
%     % Generate the crossover points. P1 and P2 are the positions of the tasks inside the sequence
%     generate_crossover_points(P1,P2),
%     prob_crossover(Pcruz),random(0.0,1.0,Pc),
%     ((Pc =< Pcruz,!,
%         cross(Ind1,Ind2,P1,P2,NInd1),
%         cross(Ind2,Ind1,P1,P2,NInd2));
%     (NInd1=Ind1,NInd2=Ind2)),
%     crossover(Rest,Rest1).

fillh([ ],[ ]).

fillh([_|R1],[h|R2]) :- fillh(R1,R2).

sublist(L1,I1,I2,L) :- I1 < I2,!,
    sublist1(L1,I1,I2,L).

    sublist(L1,I1,I2,L) :- sublist1(L1,I2,I1,L).

sublist1([X|R1],1,1,[X|H]) :- !, fillh(R1,H).

sublist1([X|R1],1,N2,[X|R2]) :- !,N3 is N2 - 1,
    sublist1(R1,1,N3,R2).

sublist1([_|R1],N1,N2,[h|R2]) :- N3 is N1 - 1,
    N4 is N2 - 1,
    sublist1(R1,N3,N4,R2).

rotate_right(L,K,L1) :- tasks(N),
    T is N - K,
    rr(T,L,L1).
    
rr(0,L,L) :- !.

rr(N,[X|R],R2) :- N1 is N - 1,
    append(R,[X],R1),
    rr(N1,R1,R2).

remove([],_,[]) :- !.

remove([X|R1],L,[X|R2]) :- not(member(X,L)),!,
    remove(R1,L,R2).

remove([_|R1],L,R2) :- remove(R1,L,R2).

insert([],L,_,L) :- !.

insert([X|R],L,N,L2) :-
    tasks(T),
    ((N>T,!,N1 is N mod T);N1 = N),
    insert1(X,N1,L,L1),
    N2 is N + 1,
    insert(R,L1,N2,L2).

insert1(X,1,L,[X|L]) :- !.

insert1(X,N,[Y|L],[Y|L1]):-
    N1 is N-1,
    insert1(X,N1,L,L1).

cross(Ind1,Ind2,P1,P2,NInd11):-
    sublist(Ind1,P1,P2,Sub1),
    tasks(NumT),
    R is NumT-P2,
    rotate_right(Ind2,R,Ind21),
    remove(Ind21,Sub1,Sub2),
    P3 is P2 + 1,
    insert(Sub2,Sub1,P3,NInd1),
    removeh(NInd1,NInd11).

removeh([],[]).

removeh([h|R1],R2) :- !,
    removeh(R1,R2).

removeh([X|R1],[X|R2]) :- removeh(R1,R2).

mutation([],[]).

mutation([Ind|Rest],[NInd|Rest1]) :-
    prob_mutation(Pmut),
    random(0.0,1.0,Pm),
    ((Pm < Pmut,!,mutacao1(Ind,NInd));NInd = Ind),
    mutation(Rest,Rest1).

mutacao1(Ind,NInd) :-
    generate_crossover_points(P1,P2),
    mutacao22(Ind,P1,P2,NInd).

mutacao22([G1|Ind],1,P2,[G2|NInd]) :-
    !, P21 is P2-1,
    mutacao23(G1,P21,Ind,G2,NInd).

mutacao22([G|Ind],P1,P2,[G|NInd]) :-
    P11 is P1-1, P21 is P2-1,
    mutacao22(Ind,P11,P21,NInd).

mutacao23(G1,1,[G2|Ind],G2,[G1|Ind]) :- !.

mutacao23(G1,P,[G|Ind],G2,[G|NInd]) :-
    P1 is P-1,
    mutacao23(G1,P1,Ind,G2,NInd).