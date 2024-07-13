% Facts - m
:- dynamic m/5.

% Facts - connects_cel
:- dynamic connects_cel/5.

% Main Rules
use_aStar(PisoOr,PisoDest,CelAct,CelDest,CustoTotal,Caminho) :-
    path_finder(PisoOr,PisoDest,_,LLig),

    elements_cells_path_aStar(PisoOr,CelAct,CelDest,LLig,Caminho,0,CustoTotal).

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

    buildings_path(EdOr,EdDest,LEdCam),

    elements_path(PisoOr,PisoDest,LEdCam,LLig).

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
        ((passage(_,_,Piso1,Piso2,CelDestAct,CelProxAct);passage(_,_,Piso2,Piso1,CelProxAct,CelDestAct)), ElemCost is 5)
    % Não precisamos de piso 2 aqui, porque o elevator é o mesmo para todos os pisos de um dado edificio
    ; Elem = elev(Piso1,Piso2) ->
        % Encontrar as células do elevator, é a mesma para todos os pisos de um dado edificio
        elevator(_,ListaPisos, CelDestAct), member(Piso1, ListaPisos), CelProxAct = CelDestAct, ElemCost is 30),

    aStar(CelAct,CelDestAct,Cam,Custo,Piso),

    % Convert Elem to a list to append to CamF
    append(Cam,[Elem],CamFTemp1),
    CustoTotal is CustoAtual + Custo + ElemCost,

    elements_cells_path_aStar(Piso2,CelProxAct,CelDest,ListaElem,CamFTemp2,CustoTotal,CustoFinal),

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

ignore(_).