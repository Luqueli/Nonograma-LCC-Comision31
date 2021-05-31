:- module(proylcc,
	[  
		put/8
	]).

:-use_module(library(lists)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% replace(?X, +XIndex, +Y, +Xs, -XsY)
%
% XsY es el resultado de reemplazar la ocurrencia de X en la posición XIndex de Xs por Y.

replace(X, 0, Y, [X|Xs], [Y|Xs]).

replace(X, XIndex, Y, [Xi|Xs], [Xi|XsY]):-
    XIndex > 0,
    XIndexS is XIndex - 1,
    replace(X, XIndexS, Y, Xs, XsY).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% put(+Contenido, +Pos, +PistasFilas, +PistasColumnas, +Grilla, -GrillaRes, -FilaSat, -ColSat).
%

put(Contenido, [RowN, ColN], PistasFilas, PistasColumnas, Grilla, NewGrilla, FilaSat, ColSat):-
	% NewGrilla es el resultado de reemplazar la fila Row en la posición RowN de Grilla
	% (RowN-ésima fila de Grilla), por una fila nueva NewRow.
	
	replace(Row, RowN, NewRow, Grilla, NewGrilla),

	% NewRow es el resultado de reemplazar la celda Cell en la posición ColN de Row por _,
	% siempre y cuando Cell coincida con Contenido (Cell se instancia en la llamada al replace/5).
	% En caso contrario (;)
	% NewRow es el resultado de reemplazar lo que se que haya (_Cell) en la posición ColN de Row por Conenido.	 
	
	(replace(Cell, ColN, _, Row, NewRow),
	Cell == Contenido 
		;
	replace(_Cell, ColN, Contenido, Row, NewRow)),

	chequearPistaColumna(ColN,PistasColumnas,NewGrilla,ColSat),
    chequearPistaFila(RowN,PistasFilas,NewRow,FilaSat)
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%

chequearPistaColumna(Col,PistasColumnas,NewGrilla,ColSat):-
		buscarColYPista(Col,PistasColumnas,NewGrilla,Columna,Pista),
    	transformList(Columna,CT),
    	comparar(CT,Pista,ColSat)
.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%

buscarColYPista(Col,LP,[],[],_Pista).

buscarColYPista(Col,LP,[T1|C1],[Elem|Aux],Pista):-
    buscarColYPista(Col,LP,C1,Aux,Pista),
    buscarDentroDeFila(Col,LP,T1,Elem,Pista)
.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%

buscarDentroDeFila(_Col,_LP,[],_Elem,_Pista).

buscarDentroDeFila(0,[P|_],[T1|_],T1,P).

buscarDentroDeFila(Col, [_|CP],[_|C1],Elem,Pista):-
         ColAux is Col - 1,
         buscarDentroDeFila(ColAux,CP,C1,Elem,Pista)
.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%

chequearPistaFila(Fila,PistasFila,NewRow,FilaSat):-
    buscarPista(Fila,PistasFila,P),
    transformList(NewRow,NRT),
    comparar(NRT,P,FilaSat)
.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%

buscarPista(0,[P|_],P).

buscarPista(Fila,[T|C],P):-
    FilaAux is Fila -1,
    buscarPista(FilaAux,C,P)
.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Metodo que compara dos listas pasadas por parametro y retorna en (-Res) un 1 si
% las listas son iguales y 0 en caso contrario.
%
comparar(T,T,1).

comparar(T,T1,0):-
	T\=T1
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Metodo que recibe un elemento +T y determina si el elemento es un "#" o una "X" o espacio vacio (_).
% Retorna, en -Res, el valor 1 si el elemento +T es un "#" y 0 en caso contrario ("X" o _).
%
subMet1(T,0):-
	T\=="#"
.

subMet1(T, 1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%

subMet2(1,TT,[1|TT]).

subMet2(0,TT,[0|TT]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%

sumar(1,[],[]).

sumar(1,[T],[T]).

sumar(1,[T|[C|D]],[X|D]):- X is T+C.

sumar(0,[T|[0|D]],[X|D]):- X is T+0.

sumar(0,TT,TT).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%
transformList([T|C],TD):-  
    transformList(C,TT),
    subMet1(T,Hashtag),
    subMet2(Hashtag,TT,TTN),
    sumar(Hashtag,TTN,TD)
.

transformList([],[]).