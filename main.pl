% Sprint B
:- dynamic sprintB_mode/0.

% Dynamic Fact - connects(Building1, Building2).
:- dynamic connects/2.


% Dynamic Fact - floors(Building, [Floors]).
:- dynamic floors/2.


% Dynamic Fact - elevator(Building, [Floors], cel(X,Y)).
:- dynamic elevator/3.


% Dynamic Fact - passage(Building1, Building2, Floor1, Floor2, cel(X1,Y1), cel(X2,Y2)).
:- dynamic passage/6.


% Dynamic Fact - m(FloorId, Col, Row, Value).
:- dynamic m/4.


% Dynamic Fact - connects_cel(FloorId, cel(Col,Lin),cel(Col,Lin),Custo).
:- dynamic connects_cel/4.

% Dynamic facts
:-dynamic generations/1.
:-dynamic population/1.
:-dynamic prob_crossover/1.
:-dynamic prob_mutation/1.
:-dynamic limit_time/1.
:-dynamic limit_stabilization/1.

% Task example: task(Task,StartFloor,EndFloor,StartCel,EndCel).
:-dynamic tasks/1.
:-dynamic task/5.
:-dynamic task_end_start_delay/3.
% Robot example: robot(Floor,Cel), robot floor and cel is the same for the start and end point
:-dynamic robot/2.
:-dynamic robot_start_task_delay/2.
:-dynamic robot_end_task_delay/2.

main_find_path(PisoO,PisoD,CelI,CelF,CamF,CustoTotal) :-
    % Activate sprint B mode
    asserta(sprintB_mode),

    retrieve_and_process_campus(),

    path_finder(PisoO,PisoD,CelI,CelF,_,_,CamF,CustoTotal),

    % Deactivate sprint B mode
    retract(sprintB_mode),

    !.

buildings_path(EdOr,EdDest,LEdCam) :-
    buildings_path2(EdOr,EdDest,[EdOr],LEdCam).

buildings_path2(EdX,EdX,LEdInv,LEdCam) :- 
    !, 
    reverse(LEdInv,LEdCam).

buildings_path2(EdAct,EdDest,LEdPassou,LEdCam) :-
    (connects(EdAct,EdInt);connects(EdInt,EdAct)),
    \+ member(EdInt,LEdPassou),
    buildings_path2(EdInt,EdDest,[EdInt|LEdPassou],LEdCam).

path_finder(PisoOr,PisoDest,CelAct,CelDest,LEdCam,LLig,CamF,CustoTotal) :- 
    floors(EdOr,LPisosOr),
    member(PisoOr,LPisosOr),
    floors(EdDest,LPisosDest),
    member(PisoDest,LPisosDest),

    % Utilizar findall para encontrar todos os caminhos possíveis entre os edifícios
    findall(LEdCamTemp,buildings_path(EdOr,EdDest,LEdCamTemp),LEdCam),

    % Processar os caminhos para encontrar um que seja possível 
    process_paths(LEdCam, LLig, PisoOr, PisoDest),

    % Valor 0 é para o custo atual
    elements_cells_path(PisoOr,CelAct,CelDest,LLig,CamF,0,CustoTotal).

% Base case: when there is only one path inside LEdCam
process_paths([Path], Result, PisoOr, PisoDest) :-
    elements_path(PisoOr,PisoDest,Path,Result), !.

% Recursive case: when there are two or more paths
process_paths([Path|OtherPaths], Result, PisoOr, PisoDest) :-
    (elements_path(PisoOr, PisoDest, Path, TempResult) -> Result = TempResult ; process_paths(OtherPaths, Result, PisoOr, PisoDest)).

% If no result is found, throw an error
process_paths([], _, _, _) :-
    throw('It was impossible to calculate the path, one of the floors was not accessible.').

elements_path(PisoDest,PisoDest,_,[]).

elements_path(PisoDest1,PisoDest,[EdDest],[elev(PisoDest1,PisoDest)]) :-
    PisoDest\==PisoDest1,
    elevator(EdDest,LPisos,_),
    member(PisoDest1,LPisos),
    member(PisoDest,LPisos).

elements_path(PisoAct,PisoDest,[EdAct,EdSeg|LOutrosEd],[cor(PisoAct,PisoSeg)|LOutrasLig]) :-
    (passage(EdAct,EdSeg,PisoAct,PisoSeg,_,_);passage(EdSeg,EdAct,PisoSeg,PisoAct,_,_)),
    elements_path(PisoSeg,PisoDest,[EdSeg|LOutrosEd],LOutrasLig).

elements_path(PisoAct,PisoDest,[EdAct,EdSeg|LOutrosEd],[elev(PisoAct,PisoAct1),cor(PisoAct1,PisoSeg)|LOutrasLig]) :-
    (passage(EdAct,EdSeg,PisoAct1,PisoSeg,_,_);passage(EdSeg,EdAct,PisoSeg,PisoAct1,_,_)),
    PisoAct1\==PisoAct,
    elevator(EdAct,LPisos,_),member(PisoAct,LPisos),member(PisoAct1,LPisos),
    elements_path(PisoSeg,PisoDest,[EdSeg|LOutrosEd],LOutrasLig).


elements_cells_path(PisoOr,CelAct,CelDest,[],CamF,CustoAtual,CustoTotal) :-

    % GET request (to DAM)
    % Only perform the GET request if the floor is not known
    (\+ m(PisoOr,_,_,_) -> retrieve_and_process_floorPlant(PisoOr);true),

    aStar(CelAct,CelDest,Cam,Custo,PisoOr),

    CamF = Cam,
    CustoTotal is CustoAtual + Custo.

elements_cells_path(PisoOr,CelAct,CelDest,[Elem|ListaElem],CamF,CustoAtual,CustoFinal) :-

    % GET request (to DAM)
    % Only perform the GET request if the floor is not known
    (\+ m(PisoOr,_,_,_) -> retrieve_and_process_floorPlant(PisoOr);true),

    % Encontrar a de destino dentro do piso
    (Elem = cor(Piso1,Piso2) ->
        % Encontrar as células do passage com base nos pisos (pode ser p1 -> p2 ou p2 -> p1)
        ((passage(_,_,Piso1,Piso2,CelDestAct,CelProxAct);passage(_,_,Piso2,Piso1,CelProxAct,CelDestAct)),ElemCost is 5)
    % Não precisamos de piso 2 aqui, porque o elevator é o mesmo para todos os pisos de um dado edificio
    ; Elem = elev(Piso1,Piso2) ->
        % Encontrar as células do elevator, é a mesma para todos os pisos de um dado edificio
        elevator(_,ListaPisos, CelDestAct), member(Piso1, ListaPisos), CelProxAct = CelDestAct,ElemCost is 30),

    aStar(CelAct,CelDestAct,Cam,Custo,PisoOr),

    % Convert Elem to a list to append to CamF
    append(Cam,[Elem],CamFTemp1),
    CustoTotal is CustoAtual + Custo + ElemCost,

    elements_cells_path(Piso2,CelProxAct,CelDest,ListaElem,CamFTemp2,CustoTotal,CustoFinal),

    append(CamFTemp1,CamFTemp2,CamF).


build_graph(_,0) :- 
    !.

build_graph(Col,Lin) :- 
    build_graph_lin(Col,Lin),Lin1 is Lin-1, !, build_graph(Col,Lin1).

build_graph_lin(0,_) :- 
    !.

build_graph_lin(Col,Lin) :- 
    m(PisoId,Col,Lin,0),!,
    ColS is Col+1, ColA is Col-1, LinS is Lin+1,LinA is Lin-1,
    % Factos - horizontal / vertical
    ((m(PisoId,ColS,Lin,0),assertz(connects_cel(PisoId,cel(Col,Lin),cel(ColS,Lin),1));true)),
    ((m(PisoId,ColA,Lin,0),assertz(connects_cel(PisoId,cel(Col,Lin),cel(ColA,Lin),1));true)),
    ((m(PisoId,Col,LinS,0),assertz(connects_cel(PisoId,cel(Col,Lin),cel(Col,LinS),1));true)),
    ((m(PisoId,Col,LinA,0),assertz(connects_cel(PisoId,cel(Col,Lin),cel(Col,LinA),1));true)),
    % Factos - diagonal
    % If sprint B mode is active, then we can move diagonally
    (sprintB_mode -> 
        ((m(PisoId,ColS, LinS, 0),m(PisoId,Col, LinS, 0),m(PisoId,ColS, Lin, 0),assertz(connects_cel(PisoId,cel(Col,Lin),cel(ColS,LinS),sqrt(2))));true),
        ((m(PisoId,ColA, LinA, 0),m(PisoId,Col, LinA, 0),m(PisoId,ColA, Lin, 0),assertz(connects_cel(PisoId,cel(Col,Lin),cel(ColA,LinA),sqrt(2))));true),
        ((m(PisoId,ColS, LinA, 0),m(PisoId,Col, LinA, 0),m(PisoId,ColS, Lin, 0),assertz(connects_cel(PisoId,cel(Col,Lin),cel(ColS,LinA),sqrt(2))));true),
        ((m(PisoId,ColA, LinS, 0),m(PisoId,ColA, Lin, 0),m(PisoId,Col, LinS, 0),assertz(connects_cel(PisoId,cel(Col,Lin),cel(ColA,LinS),sqrt(2))));true)
    ;true),
    Col1 is Col-1,
    build_graph_lin(Col1,Lin).

build_graph_lin(Col,Lin) :- 
    Col1 is Col-1,build_graph_lin(Col1,Lin).

% A*
aStar(Orig,_,_,_,_) :- 
    \+ connects_cel(_,Orig,_,_),
    throw('It was impossible to calculate the path, one of the cells was not accessible in the floor.').

aStar(_,Dest,_,_,_) :- 
    \+ connects_cel(_,_,Dest,_),
    throw('It was impossible to calculate the path, one of the cells was not accessible in the floor.').

aStar(Orig,Dest,Cam,Custo,PisoId) :-
    aStar2(Dest,[(_,0,[Orig])],Cam,Custo,PisoId).

aStar2(Dest,[(_,Custo,[Dest|T])|_],Cam,Custo,_) :-
    reverse([Dest|T],Cam).

aStar2(Dest,[(_,Ca,LA)|Outros],Cam,Custo,PisoId) :-
    LA=[Act|_],

    findall((CEX,CaX,[X|LA]),

    (Dest\==Act,(connects_cel(PisoId,Act,X,CustoX);connects_cel(PisoId,X,Act,CustoX)),
    \+ member(X,LA),
    CaX is CustoX + Ca, estimate(X,Dest,EstX),
    CEX is CaX +EstX),Novos),

    append(Outros,Novos,Todos),
    sort(Todos,TodosOrd),
    aStar2(Dest,TodosOrd,Cam,Custo,PisoId).

estimate(cel(X1,Y1),cel(X2,Y2),Estimate) :-
    Estimate is sqrt((X1-X2)^2+(Y1-Y2)^2).

% Sprint C
main_find_task_sequence_permutation(RobisepId,Sequence,Cost) :-
    % Retract sprint B mode
    retractall(sprintB_mode),

    % GET request (to DAM)
    retrieve_and_process_campus(),

    % GET request (to DAM)
    retrieve_and_process_tasks(RobisepId),

    % Calculate the times between tasks and the robot (start and end)
    calculate_times,

    % Use permutation to find the best sequence
    start_permutation(Sequence,Cost),

    !.

main_find_task_sequence_genetic_algorithm(RobisepId,Sequence,Cost) :-
    % Retract sprint B mode
    retractall(sprintB_mode),
    
    % GET request (to DAM)
    retrieve_and_process_campus(),

    % GET request (to DAM)
    retrieve_and_process_tasks(RobisepId),

    % Calculate the times between tasks and the robot (start and end)
    calculate_times,

    % Use genetic algorithm to find the best sequence
    start_genetic_algorithm(Sequence,Cost),

    !.

calculate_times :-
    % Get all tasks
    findall(T,task(T,_,_,_,_),Tasks),

    % For each pair of tasks
    forall(
        (member(T1,Tasks),member(T2,Tasks),T1\=T2),
        (
            % Calculate time from end of T1 to start of T2
            calculate_time_end_to_start_task(T1,T2)
        )
    ),

    % For each task
    foreach(
        member(T,Tasks),
        (
            % Calculate time from robot start to task start
            calculate_time_startRobot_to_startTask(T),
            % Calculate time from task end to robot end
            calculate_time_endRobot_to_endTask(T)
        )
    ).

calculate_time_end_to_start_task(T1,T2) :-
    % Get t1 info
    task(T1,_,EndFloor,_,EndCell),
    % Get t2 info
    task(T2,StartFloor,_,StartCell,_),

    % Calculate time
    path_finder(EndFloor,StartFloor,EndCell,StartCell,_,_,_,Time),

    % Assert time
    assert(task_end_start_delay(T1,T2,Time)).

calculate_time_startRobot_to_startTask(T) :-
    % Get t info
    task(T,TaskStartFloor,_,TaskStartCell,_),

    % Get robot info
    robot(RobotStartFloor,RobotStartCell),

    % Calculate time
    path_finder(RobotStartFloor,TaskStartFloor,RobotStartCell,TaskStartCell,_,_,_,Time),

    % Assert time
    assert(robot_start_task_delay(T,Time)).

calculate_time_endRobot_to_endTask(T) :-
    % Get t info
    task(T,_,TaskEndFloor,_,TaskEndCell),

    % Get robot info
    robot(RobotEndFloor,RobotEndCell),

    % Calculate time
    path_finder(TaskEndFloor,RobotEndFloor,TaskEndCell,RobotEndCell,_,_,_,Time),

    % Assert time
    assert(robot_end_task_delay(T,Time)).

% To generate a population of individuals
generate_population(Pop) :-
    population(PopSize),
    tasks(NumT),
    list_with_all_tasks(TasksList),
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
    evaluate(Ind,V,0),
    % Recursive call to evaluate the rest of the population
    evaluate_population(Rest,Rest1).

% Base case for the evaluation of an individual
evaluate([ ],0,_).

% Base case for the evaluation of an individual
evaluate([T],V,Turn) :-
    % If turn is 0, the previous task is the robot start point
    (Turn == 0 -> robot_start_task_delay(T,TimeR1);TimeR1 = 0),
    % Get the robot end point to end task delay
    robot_end_task_delay(T,TimeR2),
    % Calculate the total cost of the individual
    V is TimeR1+TimeR2.

% Recursive case for the evaluation of an individual
evaluate(IndPop,V,Turn) :-
    % Get the first task of the individual
    IndPop = [T1|Rest],
    % If turn is 0, the previous task is the robot start point
    (Turn == 0 -> robot_start_task_delay(T1,TimeR);TimeR = 0),
    % Increment the turn
    Turn1 is Turn+1,
    % Get the second task of the individual
    Rest = [T2|_],
    % To get the task information
    task_end_start_delay(T1,T2,Time),
    % Recursive call to evaluate the rest of the individual
    evaluate(Rest,V1,Turn1),
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

% Base case for limit stabilization
generate_generation(_,_,_,_,_,StabCounter,_) :-
    % Get the limit stabilization
    limit_stabilization(LimitStabilization),
    % Check if the limit stabilization has been reached
    StabCounter >= LimitStabilization.

% Base case for limit time
generate_generation(_,_,_,InitialTime,LimitTime,_,_) :- 
    % Get current time
    get_time(CurrentTime),
    % Calculate the passed time
    PassedTime is CurrentTime - InitialTime,
    % Check if the time limit has been reached
    PassedTime >= LimitTime.

% Base case for the number of generations
generate_generation(G,G,_,_,_,_,_) :- !.

% Recursive case
generate_generation(N,G,Pop,InitialTime,LimitTime,CounterPopStab,CounterEle1Stab):-
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

% Permutation
start_permutation(Sequence,Cost) :- 
    generate_permutation(Sequence,Cost), !.

delete_element(X,[X|L],L).
delete_element(X,[Y|L],[Y|L1]):-delete_element(X,L,L1).

sequence_permutation([],[]).
sequence_permutation(L,[X|L1]):-delete_element(X,L,Li),sequence_permutation(Li,L1).

update_best_sequence(Sequence) :-
    best_sequence(_, BestDuration),

    evaluate(Sequence, CurrentDuration, 0),

    (CurrentDuration < BestDuration, !,
    retract(best_sequence(_, _)),
    assertz(best_sequence(Sequence, CurrentDuration));
    true).

generate_permutation(Tasks) :-
    % High value for the first best sequence
    assertz(best_sequence([], 10000000)),
    sequence_permutation(Tasks, Sequence),
    update_best_sequence(Sequence),
    fail.

generate_permutation(_).

generate_permutation(Sequence,Cost) :-
    % Get all the tasks
    list_with_all_tasks(Tasks),
    % Generate all the permutations
    generate_permutation(Tasks),
    % Get the best sequence
    best_sequence(BestSequence, BestDuration),
    % Append 'Robot base' to the beggining of the sequence
    append([robot_base],BestSequence,SequenceTemp),
    % Append 'Robot base' to the end of the sequence
    append(SequenceTemp,[robot_base],Sequence),
    % Assign the best sequence duration to the cost
    Cost = BestDuration.

% Genetic algorithm
start_genetic_algorithm(Sequence,Cost) :-
    generate_genetic_algorithm(Sequence,Cost), !.

generate_genetic_algorithm(Sequence,Cost) :-
    initialize(50,0.7,0.01,10,10),
    generate_population(Pop),
    evaluate_population(Pop,PopValue),
    order_population(PopValue,PopOrd),
    generations(NG),
    limit_time(LimitTime),
    get_time(InitialTime),
    generate_generation(0,NG,PopOrd,InitialTime,LimitTime,0,0),
    % Get the first individual of the population
    PopOrd = [Ind*C|_],
    % Append 'Robot base' to the beggining of the sequence
    append([robot_base],Ind,SequenceTemp),
    % Append 'Robot base' to the end of the sequence
    append(SequenceTemp,[robot_base],Sequence),
    % Assign the best individual value to the cost
    Cost = C.

initialize(G,PC,PM,TL,E) :-
    asserta(generations(G)),
    tasks(NumT),
    % Population size is two times the number of tasks (unless there is only one or two tasks)
    (NumT =< 2,!,PS is NumT;PS is NumT*2),
    asserta(population(PS)),
    asserta(prob_crossover(PC)),
    asserta(prob_mutation(PM)),
    asserta(limit_time(TL)),
    asserta(limit_stabilization(E)).

% Returns a list with all the tasks
list_with_all_tasks(TasksList) :-
    findall(Task,task(Task,_,_,_,_),TasksList).
