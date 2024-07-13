:- dynamic data_loaded/0.

% Load algorithms
:- ['sprintBAlgorithm.pl'].
:- ['sprintCAlgorithm.pl'].

% Load campus
:- ['campus.pl'].

% Load maps
:- ['pisoA1.pl'].
:- ['pisoB1.pl'].
:- ['pisoB2.pl'].
:- ['pisoC1.pl'].
:- ['pisoC2.pl'].
:- ['pisoRobot1.pl'].
:- ['tarefas.pl'].

load :-
    load_a1,
    load_b1,
    load_b2,
    load_c1,
    load_c2,
    load_r1,
    load_tasks.

calculate_time_end_to_start_task(T1,T2) :-
    % Get t1 info
    task(T1,_,EndFloor,_,EndCell),
    % Get t2 info
    task(T2,StartFloor,_,StartCell,_),
    % Calculate time
    call_aStar(EndFloor,StartFloor,EndCell,StartCell,Time,_),
    % Assert time
    assert(task_end_start_delay(T1,T2,Time)).

calculate_time_startRobot_to_startTask(T) :-
    % Get t info
    task(T,TaskStartFloor,_,TaskStartCell,_),
    % Get robot info
    robot(RobotStartFloor,RobotStartCell),
    % Calculate time
    call_aStar(RobotStartFloor,TaskStartFloor,RobotStartCell,TaskStartCell,Time,_),
    % Assert time
    assert(robot_start_task_delay(T,Time)).

calculate_time_endRobot_to_endTask(T) :-
    % Get t info
    task(T,_,TaskEndFloor,_,TaskEndCell),
    % Get robot info
    robot(RobotEndFloor,RobotEndCell),
    % Calculate time
    call_aStar(TaskEndFloor,RobotEndFloor,TaskEndCell,RobotEndCell,Time,_),
    % Assert time
    assert(robot_end_task_delay(T,Time)).

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

% Call aStar
call_aStar(OriginFloor,DestinationFloor,OriginCel,DestinationCel,TotalCost,Path) :-
    use_aStar(OriginFloor,DestinationFloor,OriginCel,DestinationCel,TotalCost,Path).

% Main function
task_sequence_genetic_algorithm(NG,PS,P1,P2,TL,EL,SL,FEL) :-
    (   data_loaded
    ->  % Data already loaded, skip load and calculate_times
        start_genetic(NG,PS,P1,P2,TL,EL,SL,FEL)
    ;   % Data not loaded, perform load and calculate_times
        load,
        calculate_times,
        % Indicate data has been loaded
        assert(data_loaded),
        start_genetic(NG,PS,P1,P2,TL,EL,SL,FEL), !
    ).

best_task_sequence_permutation :-
    (   data_loaded
    ->  % Data already loaded, skip load and calculate_times
        start_permutation
    ;   % Data not loaded, perform load and calculate_times
        load,
        calculate_times,
        % Indicate data has been loaded
        assert(data_loaded),
        start_permutation, !
    ).
