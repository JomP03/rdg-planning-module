% Load algorithms
:- ['algoritmosCaminhos.pl'].

% Load campus
:- ['campus.pl'].

% Load maps
:- ['pisoA1.pl'].
:- ['pisoA2.pl'].
:- ['pisoB1.pl'].
:- ['pisoC1.pl'].
:- ['pisoC2.pl'].
:- ['pisoC3.pl'].
:- ['pisoD1.pl'].
:- ['pisoE2.pl'].
:- ['pisoE3.pl'].

load :-
    load_a1,
    load_a2,
    load_b1,
    load_c1,
    load_c2,
    load_c3,
    load_d1,
    load_e2,
    load_e3.


% Chamada ao A*
final_path_aStar(PisoO,PisoD,CelI,CelF,CustoTotal) :-
    retractall(connects_cel(_,_,_,_,_)),

    load,

    use_aStar(PisoO,PisoD,CelI,CelF,LEdCam,LLig,CamF,CustoTotal),

    print_list(CamF),nl,

    ignore(LEdCam),

    ignore(LLig).


% Chamada ao BFS
final_path_bfs(PisoO,PisoD,CelI,CelF) :-
    retractall(connects_cel(_,_,_,_,_)),

    load,

    use_bfs(PisoO,PisoD,CelI,CelF,LEdCam,LLig,CamF),

    print_list(CamF),nl,

    ignore(LEdCam),

    ignore(LLig).


% Chamada ao DFS
final_path_dfs(PisoO,PisoD,CelI,CelF) :-
    retractall(connects_cel(_,_,_,_,_)),

    load,

    use_dfs(PisoO,PisoD,CelI,CelF,LEdCam,LLig,CamF),

    print_list(CamF),nl,

    ignore(LEdCam),

    ignore(LLig).
    