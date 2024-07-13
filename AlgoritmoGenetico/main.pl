% Dynamic facts
:-dynamic generations/1.
:-dynamic population/1.
:-dynamic prob_crossover/1.
:-dynamic prob_mutation/1.
:-dynamic limit_time/1.
:-dynamic limit_evaluation/1.
:-dynamic limit_stabilization/1.
:-dynamic limit_first_element_value_stabilization/1.

% Task
task(t1).
task(t2).
task(t3).
task(t4).
task(t5).

% Robot start point to start task delay
robot_start_task_delay(r1,t1,6).
robot_start_task_delay(r1,t2,1).
robot_start_task_delay(r1,t3,2).
robot_start_task_delay(r1,t4,3).
robot_start_task_delay(r1,t5,2).

% Robot end point to end task delay
robot_end_task_delay(r1,t1,1).
robot_end_task_delay(r1,t2,3).
robot_end_task_delay(r1,t3,3).
robot_end_task_delay(r1,t4,4).
robot_end_task_delay(r1,t5,1).

% Task delay
task_end_start_delay(t1,t2,1).
task_end_start_delay(t1,t3,2).
task_end_start_delay(t1,t4,3).
task_end_start_delay(t1,t5,4).
task_end_start_delay(t2,t1,1).
task_end_start_delay(t2,t3,1).
task_end_start_delay(t2,t4,2).
task_end_start_delay(t2,t5,3).
task_end_start_delay(t3,t1,2).
task_end_start_delay(t3,t2,1).
task_end_start_delay(t3,t4,1).
task_end_start_delay(t3,t5,2).
task_end_start_delay(t4,t1,3).
task_end_start_delay(t4,t2,2).
task_end_start_delay(t4,t3,1).
task_end_start_delay(t4,t5,1).
task_end_start_delay(t5,t1,4).
task_end_start_delay(t5,t2,3).
task_end_start_delay(t5,t3,2).
task_end_start_delay(t5,t4,1).

% task(Id,ProcessTime,DueTime,PenaltyWeight).
% task(t1,2,5,1).
% task(t2,4,7,6).
% task(t3,1,11,2).
% task(t4,3,9,3).
% task(t5,3,8,2).

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
    (retract(prob_mutation(_));true), asserta(prob_mutation(PM)),

    write('Time limit (seconds):'), read(TL),
    (retract(limit_time(_));true), asserta(limit_time(TL)),

    write('Evaluation limit:'), read(EL),
    (retract(limit_evaluation(_));true), asserta(limit_evaluation(EL)),

    write('Stabilization limit:'), read(SL),
    (retract(limit_stabilization(_));true), asserta(limit_stabilization(SL)),

    write('First element value stabilization limit:'), read(FEL),
    (retract(limit_first_element_value_stabilization(_));true), asserta(limit_first_element_value_stabilization(FEL)).

generate :-
    initialize,
    generate_population(Pop),
    write('Pop='), write(Pop), nl,
    evaluate_population(Pop,PopValue),
    write('PopValue='), write(PopValue), nl,
    order_population(PopValue,PopOrd),
    generations(NG),
    limit_time(LimitTime),
    get_time(InitialTime),
    generate_generation(0,NG,PopOrd,InitialTime,LimitTime,0,0).

% To generate a population of individuals
generate_population(Pop) :-
    population(PopSize),
    tasks(NumT),
    findall(Task,task(Task),TasksList),
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

% Base case for the evaluation of an individual
evaluate([ ],0).

% Base case for the evaluation of an individual
evaluate([T],V) :-
    % Get the robot end point to end task delay
    robot_end_task_delay(r1,T,TimeR1),
    % Calculate the total cost of the individual
    V is TimeR1.

% Recursive case for the evaluation of an individual
evaluate(IndPop,V) :-
    % Get tasks size
    tasks(NumT),
    % If it is the first evaluation, the previous task is the robot start point
    length(IndPop, Length),
    (Length == NumT,robot_start_task_delay(r1,T1,TimeR);TimeR = 0),
    % Get the first task of the individual
    IndPop = [T1|Rest],
    % Get the second task of the individual
    Rest = [T2|_],
    % To get the task information
    task_end_start_delay(T1,T2,Time),
    % Recursive call to evaluate the rest of the individual
    evaluate(Rest,V1),
    % Calculate the total cost of the individual
    V is V1+Time+TimeR.
    
% Base case for the total cost of the population
population_total_cost([],0).

% Recursive case for the total cost of the population
population_total_cost([_*V|Rest],TotalCost) :-
    population_total_cost(Rest,TotalCost1),
    TotalCost is TotalCost1+V.

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

% Base case for limit evaluation
generate_generation(N,_,Pop,_,_,_,_) :-
    % Get first individual of the population evaluation
    Pop = [_*V1|_],
    % Get the limit evaluation
    limit_evaluation(LimitEvaluation),
    % Check if the limit evaluation has been reached
    V1 =< LimitEvaluation,
    % Write the limit evaluation has been reached
    write('|Limit evaluation has been reached|'), nl,
    % Write the generation
    write('Generation '), write(N), write(':'), nl, write(Pop), nl.

% Base case for limit stabilization
generate_generation(N,_,Pop,_,_,StabCounter,_) :-
    % Get the limit stabilization
    limit_stabilization(LimitStabilization),
    % Check if the limit stabilization has been reached
    StabCounter >= LimitStabilization,
    % Write the limit stabilization has been reached
    write('|Limit stabilization has been reached|'), nl,
    % Write the generation
    write('Generation '), write(N), write(':'), nl, write(Pop), nl.

% Base case for limit first element value stabilization
generate_generation(N,_,Pop,_,_,_,StabCounter) :-
    % Get the limit first element value stabilization
    limit_first_element_value_stabilization(LimitFirstElementValueStabilization),
    % Check if the limit first element value stabilization has been reached
    StabCounter >= LimitFirstElementValueStabilization,
    % Write the limit first element value stabilization has been reached
    write('|Limit first element value stabilization has been reached|'), nl,
    % Write the generation
    write('Generation '), write(N), write(':'), nl, write(Pop), nl.

% Base case for limit time
generate_generation(N,_,Pop,InitialTime,LimitTime,_,_) :- 
    % Get current time
    get_time(CurrentTime),
    % Calculate the passed time
    PassedTime is CurrentTime - InitialTime,
    % Check if the time limit has been reached
    PassedTime >= LimitTime,
    % Write the time limit has been reached
    write('|Time limit has been reached|'), nl,
    % Write the generation
    write('Generation '), write(N), write(':'), nl, write(Pop), nl.

% Base case for the number of generations
generate_generation(G,G,Pop,_,_,_,_) :- !,
    % Write the last generation
    write('Generation '), write(G), write(':'), nl, write(Pop), nl.

% Recursive case
generate_generation(N,G,Pop,InitialTime,LimitTime,CounterPopStab,CounterEle1Stab):-
    % Write the generation
    write('Generation '), write(N), write(':'), nl, write(Pop), nl,
    % Pick the first individual of the population
    Pop = [Ind1|_],
    % Pop without the first individual
    Pop = [_|PopNoFirst],
    % Randomize the order of the population and crossover
    random_permutation(Pop,Pop1),
    % Crossover the population
    crossover(Pop1,NPop),
    % Mutate the population
    mutation(NPop,NPop1),
    % Remove the cost from Pop
    remove_cost(Pop,PopNoCost),
    % Filter the new population, only keep individuals that have been crossed or mutated
    filter_population(PopNoCost,NPop1,NPop1Filtered),
    % Evaluate the new population
    evaluate_population(NPop1Filtered,NPopValue),
    % Merge the old population (without the first element) with the new population (filtered and evaluated)
    append(PopNoFirst,NPopValue,ParentsAndChildrenValue),
    % Get the population size
    population(PopSize),
    % Apply roulette selection to select the new population
    roulette_selection(ParentsAndChildrenValue,PopSize,NPopValueSelected),
    % Append the first individual of the population (ensure that the best individual is always selected)
    append([Ind1],NPopValueSelected,NPopValueSelected1),
    % Order the population according to the fitness value, ascending order
    order_population(NPopValueSelected1,NPopOrd),
    % Compare the current population with the previous one
    ((compare(Result,Pop,NPopOrd), Result == (=), !, CounterPopStab1 is CounterPopStab+1);CounterPopStab1 is 0),
    % Get the first individual of the population
    NPopOrd = [Ind2|_],
    % Compare the first individual of the population with the previous one
    ((compare(Result,Ind1,Ind2), Result == (=), !, CounterEle1Stab1 is CounterEle1Stab+1);CounterEle1Stab1 is 0),
    N1 is N+1,
    generate_generation(N1,G,NPopOrd,InitialTime,LimitTime,CounterPopStab1,CounterEle1Stab1).

remove_cost([],[]).

remove_cost([Ind1*_|Rest],[Ind1|Rest1]) :- remove_cost(Rest,Rest1).

% Filter_population predicate
filter_population(_, [], []).
filter_population(Pop, [H|T], FilteredNPop) :-
    (member(H, Pop) ->
        filter_population(Pop, T, FilteredNPop);
        filter_population(Pop, T, Temp),
        !,
        FilteredNPop = [H|Temp]).


generate_crossover_points(P1,P2) :- generate_crossover_points1(P1,P2).

generate_crossover_points1(P1,P2) :-
    tasks(N),
    NTemp is N+1,
    random(1,NTemp,P11),
    random(1,NTemp,P21),
    P11\==P21,!,
    ((P11<P21,!,P1=P11,P2=P21);P1=P21,P2=P11).

generate_crossover_points1(P1,P2) :- generate_crossover_points1(P1,P2).

% Base case when the length of Indivs is 0 or 1, no work needed
crossover([ ],[ ]).

crossover([Ind*_],[Ind]).

crossover([Ind1*_,Ind2*_|Rest],[NInd1,NInd2|Rest1]) :-
    % Generate the crossover points. P1 and P2 are the positions of the tasks inside the sequence
    generate_crossover_points(P1,P2),
    prob_crossover(Pcruz),random(0.0,1.0,Pc),
    ((Pc =< Pcruz,!,
        cross(Ind1,Ind2,P1,P2,NInd1),
        cross(Ind2,Ind1,P1,P2,NInd2));
    (NInd1=Ind1,NInd2=Ind2)),
    crossover(Rest,Rest1).

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

% Create a list with the probabilities of each individual
create_probabilities([],_,[]).

create_probabilities([_*V|Rest],TotalCost,[V1|Rest1]) :-
    V1 is V/TotalCost,
    create_probabilities(Rest,TotalCost,Rest1).

% Create a list with the cumulative probabilities of each individual
create_cumulative_probabilities(Pop, TotalCost, CumulativeProbList) :-
    create_probabilities(Pop, TotalCost, ProbList),
    cumulative_sum(ProbList, CumulativeProbList).

% Calculate the cumulative sum of a list
cumulative_sum(List, CumulativeSumList) :-
    cumulative_sum_helper(List, 0, CumulativeSumList).

cumulative_sum_helper([], _, []).
cumulative_sum_helper([H|T], Sum, [NewSum|CumulativeSumTail]) :-
    NewSum is Sum + H,
    cumulative_sum_helper(T, NewSum, CumulativeSumTail).

% Select the individual using the cumulative probabilities and remove it from the population
roulette_selection1([_|Rest], [P|_], R, Rest) :- R =< P, !.

% Added this method because if the individual is not selected, it should be added to the resulting population
roulette_selection1([X|Rest], [P|ProbList], R, [X|NewPop]) :-
    R > P,
    roulette_selection1(Rest, ProbList, R, NewPop).

% Call the helper function (works as a wrapper)
roulette_selection(Pop, PopSize, FinalPop) :-
    % Call the helper function
    roulette_selection_helper(Pop, PopSize, FinalPop).

roulette_selection_helper(Pop, PopSize, Pop) :-
    length(Pop, Length),
    Length =:= PopSize - 1, !.

roulette_selection_helper(Pop, PopSize, FinalPop) :-
    % Get the population total cost and create a list with the cumulative probabilities of each individual
    roulette_selection_helper1(Pop, CumulativeProbList),
    % Generate a random number between 0 and 1
    random(0.0, 1.0, R),
    % Select and remove the next individual
    roulette_selection1(Pop, CumulativeProbList, R, NewPop),
    % Recursive call
    roulette_selection_helper(NewPop, PopSize, FinalPop).

% Added this for modularization purposes

roulette_selection_helper1(Pop, CumulativeProbList) :-
    % Get the population total cost
    population_total_cost(Pop, TotalCost),
    % Create a list with the cumulative probabilities of each individual
    create_cumulative_probabilities(Pop, TotalCost, CumulativeProbList),
    !.
