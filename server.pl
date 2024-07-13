% Load the main.pl file
:- [main].


% Libraries
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_cors)).


% JSON Libraries
:- use_module(library(http/json_convert)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).


%Cors
:- set_setting(http:cors, [*]).

% Server fact
:- dynamic server/1.


% Handlers
:- http_handler('/api/prolog/paths', retrieve_path, []).

:- http_handler('/api/prolog/taskSequencePermutation', retrieve_task_sequence_permutation, []).

:- http_handler('/api/prolog/taskSequenceGeneticAlgorithm', retrieve_task_sequence_genetic_algorithm, []).

% Handlers
retrieve_path(Request) :-
    catch(
        (
            http_parameters(Request,
                            [ originFloor(OriginFloor, []),
                              destinationFloor(DestinationFloor, []),
                              originCel(OriginCel, []),
                              destinationCel(DestinationCel, [])
                            ]),
            
            term_string(A, OriginFloor),
            term_string(B, DestinationFloor),
            term_string(C, OriginCel),
            term_string(D, DestinationCel),

            main_find_path(A, B, C, D, Path, Cost),

            % Send the response in JSON format
            format('Content-type: application/json~n~n'),
            prolog_list_to_json(Path, PathJSON),
            format('{"path": ~w, "cost": ~w}', [PathJSON, Cost])

        ),
        E,
        handle_error(E)
    ).

retrieve_task_sequence_permutation(Request) :-
    catch(
        (
            http_parameters(Request,
                            [ robisepId(RobisepId, [])
                            ]),
            
            term_string(A, RobisepId),

            main_find_task_sequence_permutation(A, Sequence, Cost),

            % Send the response in JSON format
            format('Content-type: application/json~n~n'),
            prolog_list_to_json(Sequence, PathJSON),
            format('{"Sequence": ~w, "cost": ~w}', [PathJSON, Cost])

        ),
        E,
        handle_error(E)
    ).

retrieve_task_sequence_genetic_algorithm(Request) :-
    catch(
        (
            http_parameters(Request,
                            [ robisepId(RobisepId, [])
                            ]),
            
            term_string(A, RobisepId),

            main_find_task_sequence_genetic_algorithm(A, Sequence, Cost),

            % Send the response in JSON format
            format('Content-type: application/json~n~n'),
            prolog_list_to_json(Sequence, PathJSON),
            format('{"Sequence": ~w, "cost": ~w}', [PathJSON, Cost])

        ),
        E,
        handle_error(E)
    ).

% Error handler
handle_error(E) :-
    format('Status: 400 Bad Request~n'),
    format('Content-type: application/json~n~n'),
    format('{"error": "~w"}', [E]).

% URLS
to_campus("https://vs-gate.dei.isep.ipp.pt:11071/api/prolog/campus").

to_floorPlan(URL, FloorId) :-
    Default = "https://vs-gate.dei.isep.ipp.pt:11071/api/prolog/floorPlants/",
    format(atom(FloorIdAtom), "~w", FloorId),
    atomic_list_concat([Default, FloorIdAtom], URL).

to_tasks(URL, RobisepId) :-
    Default = "https://vs-gate.dei.isep.ipp.pt:11071/api/prolog/tasks/",
    format(atom(RobisepIdAtom), "~w", RobisepId),
    atomic_list_concat([Default, RobisepIdAtom], URL).

% Start the server at port: Port
server(Port) :-
    http_server(http_dispatch, 
        [ port(Port),
            workers(16)
        ]),
    assert(server(Port)).

% Stop the server
stop_server(Port) :-
    retract(server(Port)),
    http_stop_server(Port, []).


% Campus information
% Assert the floors
assertFloors(floors=[]):-!.

assertFloors(floors=[Content| Rest]):-
        term_string(floors(A,B),Content),
        assert(floors(A,B)),
        assertFloors(floors=Rest).


% Assert the elevators
assertElevators(elevators=[]):-!.

assertElevators(elevators=[Content| Rest]):-
    term_string(elevator(A,B,C),Content),
    assert(elevator(A,B,C)),
    assertElevators(elevators=Rest).


% Assert the passages
assertPassages(passages=[]):-!.

assertPassages(passages=[Content| Rest]):-
    term_string(passage(A,B,C,D,E,F),Content),
    assert(passage(A,B,C,D,E,F)),
    assertPassages(passages=Rest).


% Assert the connects
assertConnects(connects=[]):-!.

assertConnects(connects=[Content| Rest]):-
    term_string(connects(A,B),Content),
    assert(connects(A,B)),
    assertConnects(connects=Rest).


parseCampus(json([Floors,Elevators,Passages,Connects])):-
    assertFloors(Floors),
    assertElevators(Elevators),
    assertPassages(Passages),
    assertConnects(Connects).

% Predicate to retrieve and process JSON data
retrieve_and_process_campus() :-

    retractall(floors(_,_)),
    retractall(elevator(_,_,_)),
    retractall(passage(_,_,_,_,_,_)),
    retractall(connects(_,_)),

    % Open HTTP connection to retrieve JSON data
    to_campus(URL),
    catch(
        http_open(URL, In, []),
        Error,
        handle_http_error(Error)
    ),

    % Read and parse JSON data
    (   var(Error)
    ->  json_read(In, JsonData),
        close(In),
        parseCampus(JsonData)
    ;   true % handle_http_error/1 already handled the error
    ).

% Floor Plan / Graph height and width
assertPlan(floorPlanCells=[]):- !.

assertPlan(floorPlanCells=[ Content | Rest]):-
    term_string(m(A,B,C,D),Content),
    assert(m(A,B,C,D)),
    assertPlan(floorPlanCells=Rest).


getHeight(floorPlanHeight=H, H):-!.

getWidth(floorPlanWidth=W, W):-!.

create_graph(Height,Width):-
    getHeight(Height, H),
    getWidth(Width, W),
    build_graph(W,H).

parseFloorPlan(json([Height,Width,Plan])):-
    assertPlan(Plan),
    create_graph(Height,Width).
    

retrieve_and_process_floorPlant(FloorId) :-
    retractall(m(_,_,_,_)),

    % Only retract if sprint B mode is active
    (   sprintB_mode ->
            retractall(connects_cel(_,_,_,_))
        ;   true
    ),

    % Open HTTP connection to retrieve JSON data
    to_floorPlan(URL,FloorId),
    catch(
        http_open(URL, In, []),
        Error,
        handle_http_error(Error)
    ),

    % Read and parse JSON data
    (   var(Error)
    ->  json_read(In, JsonData),
        close(In),
        parseFloorPlan(JsonData)
    ;   true % handle_http_error/1 already handled the error
    ).

% Tasks
assertRobot(robot=Content):-
    term_string(robot(A,B),Content),
    assert(robot(A,B)).

assertTasks(tasks=[]):-!.

assertTasks(tasks=[Content| Rest]):-
    term_string(task(A,B,C,D,E),Content),
    assert(task(A,B,C,D,E)),
    assertTasks(tasks=Rest).

parseTasks(json([Tasks,Robot])):-
    assertRobot(Robot),
    assertTasks(Tasks),
    % Use findall to get tasks length
    findall(_, task(_,_,_,_,_), TasksList),
    length(TasksList, TasksLength),
    % Assert the tasks length if it is bigger than 0, else throw an error
    (   TasksLength > 0
    ->  assert(tasks(TasksLength))
    ;   throw('No tasks found')
    ).

retrieve_and_process_tasks(RobisepId) :-
    % Retract all tasks
    retractall_tasks,

    % Open HTTP connection to retrieve JSON data
    to_tasks(URL,RobisepId),
    catch(
        http_open(URL, In, []),
        Error,
        handle_http_error(Error)
    ),

    % Read and parse JSON data
    (   var(Error)
    ->  json_read(In, JsonData),
        close(In),
        parseTasks(JsonData)
    ;   true % handle_http_error/1 already handled the error
    ).

% Utils
handle_http_error(error(existence_error(url, URL), _Context)) :-
  format('Custom error message: Resource with the following URL does not exist: ~w', [URL]),
  nl,!.

prolog_list_to_json(List, JSON) :-
    maplist(term_string, List, StringList),
    atom_json_term(JSON, StringList, []).

retractall_tasks :-
    retractall(connects_cel(_,_,_,_)),
    retractall(m(_,_,_,_)),
    retractall(tasks(_)),
    retractall(task(_,_,_,_,_)),
    retractall(task_end_start_delay(_,_,_,_,_)),
    retractall(robot(_,_)),
    retractall(robot_start_task_delay(_,_)),
    retractall(robot_end_task_delay(_,_)),
    retractall(generations(_)),
    retractall(population(_)),
    retractall(prob_crossover(_)),
    retractall(prob_mutation(_)),
    retractall(limit_time(_)),
    retractall(limit_stabilization(_)).