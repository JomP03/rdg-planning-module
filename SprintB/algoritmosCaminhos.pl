% Facts - m
:- dynamic m/5.

% Facts - connects_cel
:- dynamic connects_cel/5.

% Main Rules
use_aStar(PisoOr,PisoDest,CelAct,CelDest,LEdCam,LLig,CamF,CustoTotal) :-
    path_finder(PisoOr,PisoDest,LEdCam,LLig),

    elements_cells_path_aStar(PisoOr,CelAct,CelDest,LLig,CamF,0,CustoTotal),

    ignore(LEdCam),

    ignore(LLig).

use_bfs(PisoOr,PisoDest,CelAct,CelDest,LEdCam,LLig,CamF) :-
    path_finder(PisoOr,PisoDest,LEdCam,LLig),

    elements_cells_path_bfs(PisoOr,CelAct,CelDest,LLig,CamF),

    ignore(LEdCam),

    ignore(LLig).

use_dfs(PisoOr,PisoDest,CelAct,CelDest,LEdCam,LLig,CamF) :-
    path_finder(PisoOr,PisoDest,LEdCam,LLig),

    elements_cells_path_dfs(PisoOr,CelAct,CelDest,LLig,CamF),

    ignore(LEdCam),

    ignore(LLig).



% For the determination of the best path using minimum number of elevators
best_path_floors(PisoOr,PisoDest,LLigMelhor) :-
    findall(LLig,path_finder(PisoOr,PisoDest,_,LLig),LLLig),
    fewer_elevators(LLLig,LLigMelhor,_,_).

fewer_elevators([LLig],LLig,NElev,NCor) :- count(LLig,NElev,NCor).

fewer_elevators([LLig|OutrosLLig],LLigR,NElevR,NCorR) :-
    fewer_elevators(OutrosLLig,LLigM,NElev,NCor),

    count(LLig,NElev1,NCor1),

    (((NElev1<NElev;(NElev1==NElev,NCor1<NCor)),!,

    NElevR is NElev1, NCorR is NCor1,LLigR=LLig);
    
    (NElevR is NElev,NCorR is NCor,LLigR=LLigM)).

    count([],0,0).

    count([elev(_,_)|L],NElev,NCor) :- count(L,NElevL,NCor),NElev is NElevL+1.

    count([cor(_,_)|L],NElev,NCor) :- count(L,NElev,NCorL),NCor is NCorL+1.

% Find path between two buildings
buildings_path(EdOr,EdDest,LEdCam) :-
    buildings_path2(EdOr,EdDest,[EdOr],LEdCam).

buildings_path2(EdX,EdX,LEdInv,LEdCam) :- 
    !, 
    reverse(LEdInv,LEdCam).

buildings_path2(EdAct,EdDest,LEdPassou,LEdCam) :-
    (connects(EdAct,EdInt);connects(EdInt,EdAct)),
    \+ member(EdInt,LEdPassou),
    buildings_path2(EdInt,EdDest,[EdInt|LEdPassou],LEdCam).

% Find path between two floors
path_finder(PisoOr,PisoDest,LEdCam, LLig) :-
    floors(EdOr,LPisosOr),
    member(PisoOr,LPisosOr),
    floors(EdDest,LPisosDest),
    member(PisoDest,LPisosDest),

    (buildings_path(EdOr,EdDest,LEdCam),!;write('Nao existe caminho entre os edificios'),nl,fail),

    (elements_path(PisoOr,PisoDest,LEdCam,LLig),!;write('Nao existe caminho entre os pisos'),nl,fail).

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


% For aStar
elements_cells_path_aStar(Piso,CelAct,CelDest,[],CamF,CustoAtual,CustoTotal) :-

    aStar(CelAct,CelDest,Cam,Custo,Piso),

    CamF = Cam,
    CustoTotal is CustoAtual + Custo.

elements_cells_path_aStar(Piso,CelAct,CelDest,[Elem|ListaElem],CamF,CustoAtual,CustoFinal) :-

    % Encontrar a de destino dentro do piso
    (Elem = cor(Piso1,Piso2) ->
        % Encontrar as células do passage com base nos pisos (pode ser p1 -> p2 ou p2 -> p1)
        (passage(_,_,Piso1,Piso2,CelDestAct,CelProxAct);passage(_,_,Piso2,Piso1,CelProxAct,CelDestAct))
    % Não precisamos de piso 2 aqui, porque o elevator é o mesmo para todos os pisos de um dado edificio
    ; Elem = elev(Piso1,Piso2) ->
        % Encontrar as células do elevator, é a mesma para todos os pisos de um dado edificio
        elevator(_,ListaPisos, CelDestAct), member(Piso1, ListaPisos), CelProxAct = CelDestAct),

    aStar(CelAct,CelDestAct,Cam,Custo,Piso),

    % Convert Elem to a list to append to CamF
    append(Cam,[Elem],CamFTemp1),
    CustoTotal is CustoAtual + Custo,

    elements_cells_path_aStar(Piso2,CelProxAct,CelDest,ListaElem,CamFTemp2,CustoTotal,CustoFinal),

    append(CamFTemp1,CamFTemp2,CamF).


% For bfs
elements_cells_path_bfs(Piso,CelAct,CelDest,[],CamF) :-

    bfs(CelAct,CelDest,Cam,Piso),

    CamF = Cam.

elements_cells_path_bfs(Piso,CelAct,CelDest,[Elem|ListaElem],CamF) :-
    % Encontrar a de destino dentro do piso
    (Elem = cor(Piso1,Piso2) ->
        % Encontrar as células do passage com base nos pisos (pode ser p1 -> p2 ou p2 -> p1)
        (passage(_,_,Piso1,Piso2,CelDestAct,CelProxAct);passage(_,_,Piso2,Piso1,CelProxAct,CelDestAct))
    % Não precisamos de piso 2 aqui, porque o elevator é o mesmo para todos os pisos de um dado edificio
    ; Elem = elev(Piso1,Piso2) ->
        % Encontrar as células do elevator, é a mesma para todos os pisos de um dado edificio
        elevator(_,ListaPisos, CelDestAct), member(Piso1, ListaPisos), CelProxAct = CelDestAct),

    bfs(CelAct,CelDestAct,Cam,Piso),

    % Convert Elem to a list to append to CamF
    append(Cam,[Elem],CamFTemp1),

    elements_cells_path_bfs(Piso2,CelProxAct,CelDest,ListaElem,CamFTemp2),

    append(CamFTemp1,CamFTemp2,CamF).


% For dfs
elements_cells_path_dfs(Piso,CelAct,CelDest,[],CamF) :-
    dfs(CelAct,CelDest,Cam,Piso),

    CamF = Cam.

elements_cells_path_dfs(Piso,CelAct,CelDest,[Elem|ListaElem],CamF) :-
    % Encontrar a de destino dentro do piso
    (Elem = cor(Piso1,Piso2) ->
        % Encontrar as células do passage com base nos pisos (pode ser p1 -> p2 ou p2 -> p1)
        (passage(_,_,Piso1,Piso2,CelDestAct,CelProxAct);passage(_,_,Piso2,Piso1,CelProxAct,CelDestAct))
    % Não precisamos de piso 2 aqui, porque o elevator é o mesmo para todos os pisos de um dado edificio
    ; Elem = elev(Piso1,Piso2) ->
        % Encontrar as células do elevator, é a mesma para todos os pisos de um dado edificio
        elevator(_,ListaPisos, CelDestAct), member(Piso1, ListaPisos), CelProxAct = CelDestAct),

    dfs(CelAct,CelDestAct,Cam,Piso),

    % Convert Elem to a list to append to CamF
    append(Cam,[Elem],CamFTemp1),

    elements_cells_path_dfs(Piso2,CelProxAct,CelDest,ListaElem,CamFTemp2),

    append(CamFTemp1,CamFTemp2,CamF).
    

build_graph(_,0) :- 
    !.

build_graph(Col,Lin) :- 
    build_graph_lin(Col,Lin),Lin1 is Lin-1, !, build_graph(Col,Lin1).

build_graph_lin(0,_) :- 
    !.

build_graph_lin(Col,Lin) :- 
    m(_,Piso,Col,Lin,0),!,
    ColS is Col+1, ColA is Col-1, LinS is Lin+1,LinA is Lin-1,
    % Factos - horizontal / vertical
    ((m(_,Piso,ColS,Lin,0),assertz(connects_cel(_,Piso,cel(Col,Lin),cel(ColS,Lin),1));true)),
    ((m(_,Piso,ColA,Lin,0),assertz(connects_cel(_,Piso,cel(Col,Lin),cel(ColA,Lin),1));true)),
    ((m(_,Piso,Col,LinS,0),assertz(connects_cel(_,Piso,cel(Col,Lin),cel(Col,LinS),1));true)),
    ((m(_,Piso,Col,LinA,0),assertz(connects_cel(_,Piso,cel(Col,Lin),cel(Col,LinA),1));true)),
    % Factos - diagonal
    % ((m(_,Piso,ColS, LinS, 0),m(_,Piso,Col, LinS, 0),m(_,Piso,ColS, Lin, 0),assertz(connects_cel(_,Piso,cel(Col,Lin),cel(ColS,LinS),sqrt(2))));true),
    % ((m(_,Piso,ColA, LinA, 0),m(_,Piso,Col, LinA, 0),m(_,Piso,ColA, Lin, 0),assertz(connects_cel(_,Piso,cel(Col,Lin),cel(ColA,LinA),sqrt(2))));true),
    % ((m(_,Piso,ColS, LinA, 0),m(_,Piso,Col, LinA, 0),m(_,Piso,ColS, Lin, 0),assertz(connects_cel(_,Piso,cel(Col,Lin),cel(ColS,LinA),sqrt(2))));true),
    % ((m(_,Piso,ColA, LinS, 0),m(_,Piso,ColA, Lin, 0),m(_,Piso,Col, LinS, 0),assertz(connects_cel(_,Piso,cel(Col,Lin),cel(ColA,LinS),sqrt(2))));true),
    Col1 is Col-1,
    build_graph_lin(Col1,Lin).

build_graph_lin(Col,Lin) :- 
    Col1 is Col-1,build_graph_lin(Col1,Lin).

% A*
aStar(Orig,_,_,_,_) :- 
    \+ connects_cel(_,_,Orig,_,_),!,write('Origem inexistente'),nl,fail.

aStar(_,Dest,_,_,_) :-
    \+ connects_cel(_,_,Dest,_,_),!,write('Destino inexistente'),nl,fail.

aStar(Orig,Dest,Cam,Custo,Piso) :-
    aStar2(Dest,[(_,0,[Orig])],Cam,Custo,Piso).

aStar2(Dest,[(_,Custo,[Dest|T])|_],Cam,Custo,_) :-
    reverse([Dest|T],Cam).

aStar2(Dest,[(_,Ca,LA)|Outros],Cam,Custo,Piso) :-
    LA=[Act|_],

    findall((CEX,CaX,[X|LA]),

    (Dest\==Act,(connects_cel(_,Piso,Act,X,CustoX);connects_cel(_,Piso,X,Act,CustoX)),
    \+ member(X,LA),
    CaX is CustoX + Ca, estimate(X,Dest,EstX),
    CEX is CaX +EstX),Novos),

    append(Outros,Novos,Todos),
    sort(Todos,TodosOrd),
    aStar2(Dest,TodosOrd,Cam,Custo,Piso).

estimate(cel(X1,Y1),cel(X2,Y2),Estimate) :-
    Estimate is sqrt((X1-X2)^2+(Y1-Y2)^2).

% breadth-first search
bfs(Orig,Dest,Cam,Piso) :-  
    bfs2(Dest,[[Orig]],Cam,Piso).

 
bfs2(Dest,[[Dest|T]|_],Cam,_) :- 
    reverse([Dest|T],Cam). 


bfs2(Dest,[LA|Outros],Cam,Piso):- 

    LA=[Act|_], 

    findall([X|LA], 

    (Dest\==Act,connects_cel(_,Piso,Act,X,_),\+ member(X,LA)), 

    Novos), 

    append(Outros,Novos,Todos), 

    bfs2(Dest,Todos,Cam,Piso). 

% depth-first search
dfs(Orig,Dest,Cam,Piso):- 

    dfs2(Orig,Dest,[Orig],Cam,Piso).

 
dfs2(Dest,Dest,LA,Cam,_):- 

    reverse(LA,Cam).


dfs2(Act,Dest,LA,Cam,Piso):- 

    connects_cel(_,Piso,Act,X,_),\+ member(X,LA), 

    write(X),nl,

    dfs2(X,Dest,[X|LA],Cam,Piso). 


% Best Depth-first search
all_dfs(Orig,Dest,LCam,Piso) :- findall(Cam,dfs(Orig,Dest,Cam,Piso),LCam).

better_dfs(Orig,Dest,Cam,Piso) :- 
    all_dfs(Orig,Dest,LCam,Piso), 
    shortlist(LCam,Cam,_).

shortlist([L],L,N) :- 
    !,
    length(L,N).

shortlist([L|LL],Lm,Nm) :- 
    shortlist(LL,Lm1,Nm1),
    length(L,NL),
    ((NL<Nm1,!,Lm=L,Nm is NL);(Lm=Lm1,Nm is Nm1)).

    
% Auxiliar rules
print_list([]).
print_list([H|T]) :-
    write(H), nl,
    print_list(T).

ignore(_).