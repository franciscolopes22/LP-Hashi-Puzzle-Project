% ist199220     Francisco Lopes


% ------------------------------------------------------------------------------
% non_zero_indexes(List, Indexes)
% Indexes eh a lista de todos os indices de do elementos que nao sao zero.
% ------------------------------------------------------------------------------

non_zero_indexes(List, Indexes) :-
    findall(Index, 
            (nth0(Index, List, Element), Element =\= 0),
            Indexes).

getNumbers(L1,L2):-findall(X,(member(X,L1), X =\=0),L2).


% ------------------------------------------------------------------------------
% getIlhas(N_L, [P|R], [H|T], Ilhas, IlhasAux)
% Ilhas eh a lista de todas as ilhas num Puzzle.
% ------------------------------------------------------------------------------

getIlhas(_, [], [], Ilhas, Ilhas).
getIlhas(N_L, [P|R], [H|T], Ilhas, IlhasAux):-
    Pos is H + 1,
    append(IlhasAux, [ilha(P, (N_L, Pos))], N_Ilhas),
    getIlhas(N_L, R, T, Ilhas, N_Ilhas).


% ------------------------------------------------------------------------------
% extrai_ilhas_linha(N_L, Linha, Ilhas)
% Ilhas eh a lista de todas as ilhas num Puzzle.
% ------------------------------------------------------------------------------

extrai_ilhas_linha(N_L, Linha, Ilhas):-
    getNumbers(Linha, Nums),
    non_zero_indexes(Linha, Indexes),
    getIlhas(N_L, Nums, Indexes, Ilhas, []).


% ------------------------------------------------------------------------------
% ilhas(Puz, Ilhas)
% Ilhas eh a lista de todas as ilhas num Puzzle.
% ------------------------------------------------------------------------------

ilhas(Puz, Ilhas):-
    length(Puz, Len),
    ilhas_aux(Len, Len, Puz, Ilhas, []).

ilhas_aux(0, _,_, Ilhas, Ilhas).
ilhas_aux(Len, InitialLen, [H|T], Ilhas, IlhasAux):-
    New_Len is Len-1, 
    Line is InitialLen - New_Len,
    extrai_ilhas_linha(Line, H, N_Ilhas),
    append(IlhasAux, N_Ilhas, Ilhas_Updated),
    ilhas_aux(New_Len, InitialLen, T, Ilhas, Ilhas_Updated).


% ------------------------------------------------------------------------------
% verticalCheck(ilha(_, (A, Y)), ilha(_, (B,Y)))
% Verifica se uma ilha se encontra na mesma coluna.
% ------------------------------------------------------------------------------

verticalCheck(ilha(_, (A, Y)), ilha(_, (B,Y))):- A \= B.

% ------------------------------------------------------------------------------
% horizontalCheck(ilha(_, (X, A)), ilha(_, (X,B)))
% Verifica se uma ilha se encontra na mesma linha.
% ------------------------------------------------------------------------------

horizontalCheck(ilha(_, (X, A)), ilha(_, (X,B))):- A \= B.

% ------------------------------------------------------------------------------
% getVerticalNeighbor([H|T], Ilha, Vizinhas, Aux)
% Viznhas eh a lista de todas as ilhas na mesma coluna da Ilha.
% ------------------------------------------------------------------------------

getVerticalNeighbor([], _, Vizinhas, Vizinhas).
getVerticalNeighbor([H|T], Ilha, Vizinhas, Aux):-
    verticalCheck(H, Ilha) ->
append(Aux, [H], New_Vizinhas),
getVerticalNeighbor(T, Ilha, Vizinhas, New_Vizinhas);
getVerticalNeighbor(T, Ilha, Vizinhas, Aux).

% ------------------------------------------------------------------------------
% getHorizontalNeighbor([H|T], Ilha, Vizinhas, Aux)
% Viznhas eh a lista de todas as ilhas na mesma linha da Ilha.
% ------------------------------------------------------------------------------

getHorizontalNeighbor([], _, Vizinhas, Vizinhas).
getHorizontalNeighbor([H|T], Ilha, Vizinhas, Aux):-
    horizontalCheck(H, Ilha) ->
append(Aux, [H], New_Vizinhas),
getHorizontalNeighbor(T, Ilha, Vizinhas, New_Vizinhas);
getHorizontalNeighbor(T, Ilha, Vizinhas, Aux).

% ------------------------------------------------------------------------------
% reverse(List,Result)
% Result eh a List invertida.
% ------------------------------------------------------------------------------

reverse(List,Result) :-
    reverse(List,[],Result).
reverse([],ReversedList,ReversedList).
reverse([Head|Tail],RestTail,ReverseList) :-
     reverse(Tail,[Head|RestTail],ReverseList).

% ------------------------------------------------------------------------------
% split(List, Ilha, Before, After)
% split divide uma lista em duas, Before elementos que vem antes de Ilha, e After 
% elementos que vem depois da Ilha
% ------------------------------------------------------------------------------

split(List, Ilha, Before, After) :-
    append(Before, [Ilha|After], List).


% ------------------------------------------------------------------------------
% juntaVizinhos([[A|_]|L], [A|P])
% Dada uma Lista de Listas, Retorna uma Lista com o primeiro elemento de cada lista.
% ------------------------------------------------------------------------------

juntaVizinhos([],[]).
juntaVizinhos([[A|_]|L], [A|P]) :- juntaVizinhos(L, P).


% ------------------------------------------------------------------------------
% vizinhas(Ilhas, Ilha, Vizinhas)
% Ilhas eh a lista de ilhas de um puzzle e Ilha eh uma dessas ilhas, significa que 
% Vizinhas eh a lista ordenada (ilhas de cima para baixo e da esquerda para a direita) 
% cujos elementos sao as ilhas vizinhas de Ilha.
% ------------------------------------------------------------------------------

empty([]).

vizinhas(Ilhas, Ilha, Vizinhas):-

    split(Ilhas, Ilha, VizinhosCima, VizinhosBaixo),
 
    reverse(VizinhosCima, VizinhosCimaReversed),

    getHorizontalNeighbor(VizinhosCimaReversed, Ilha, VizinhoEsquerdo, []),

    getVerticalNeighbor(VizinhosCimaReversed, Ilha, VizinhoDeCima,[]),

    getHorizontalNeighbor(VizinhosBaixo, Ilha, VizinhoDireito, [] ),

    getVerticalNeighbor(VizinhosBaixo, Ilha, VizinhoDeBaixo, []),

    exclude(empty, [VizinhoDeCima,VizinhoEsquerdo,VizinhoDireito,VizinhoDeBaixo], L),

    juntaVizinhos(L, Vizinhas).




%estado(Ilhas, Estado).

% ------------------------------------------------------------------------------
% betweenToList((A,X),(A,Y),[(A,Z)|Xs])
% Dadas duas posicoes crescente devolve posicoes entre elas.
% ------------------------------------------------------------------------------


betweenToList((A,X),(A,X),[]) :- !.
betweenToList((A,X),(A,Y),[(A,Z)|Xs]) :-
    X < Y,
    Z is X+1,
    betweenToList((A,Z),(A,Y),Xs).
betweenToList((X,A),(Y,A),[(Z,A)|Xs]) :-
    X < Y,
    Z is X+1,
    betweenToList((Z,A),(Y,A),Xs).


% ------------------------------------------------------------------------------
% posicoes_entre((X1,Y1), (X1,Y2), Posicoes).
% Dadas duas posicoes crescente devolve posicoes entre elas de maneira crescente.
% ------------------------------------------------------------------------------

posicoes_entre((X1,Y1), (X1,Y2), Posicoes):-
    Y1 < Y2
    ->
    Y is Y2 - 1,
    betweenToList((X1, Y1), (X1, Y), Posicoes);
    Y is Y1 - 1,
    betweenToList((X1, Y2), (X1, Y), Posicoes).

posicoes_entre((X1,Y1), (X2,Y1), Posicoes):-
    X1 < X2
    ->
    X is X2 -1,
    betweenToList((X1, Y1), (X, Y1), Posicoes);
    X is X1 -1,
    betweenToList((X2, Y1), (X, Y1), Posicoes).

% ------------------------------------------------------------------------------
% estado(Ilhas, Estado).
% Estado eh a lista de estados de cada ilha.
% ------------------------------------------------------------------------------


estado(Ilhas, Estado):-
    estado_aux(Ilhas, Ilhas, Estado).

estado_aux(_, [], []).
estado_aux(Ilhas,[H|T], [[H, Vizinhas, []]|Doubled_list]):-
    vizinhas(Ilhas, H, Vizinhas),
    estado_aux(Ilhas, T, Doubled_list).
    



% ------------------------------------------------------------------------------
% cria_ponte(Ilha1, Ilha2).
% Cria uma ponte entfe duas ilhas.
% ------------------------------------------------------------------------------


cria_ponte((X1, Y1), (X2, Y2), ponte((X1, Y1), (X2, Y2))):- X1 < X2.
cria_ponte((X1, Y1), (X2, Y2), ponte((X2, Y2), (X1, Y1))):- X1 > X2.
cria_ponte((X1, Y1), (X1, Y2), ponte((X1, Y1), (X1, Y2))):- Y1 < Y2.
cria_ponte((X1, Y1), (X1, Y2), ponte((X1, Y2), (X1, Y1))):- Y1 > Y2.



% ------------------------------------------------------------------------------
% caminho_livre(Pos1, Pos2, Posicoes, I, Vz).
% em que Pos1 e Pos2 sao posicoes, Posicoes eh a lista ordenada de posicoes 
% entre Pos1 e Pos2, I eh uma ilha, e Vz eh uma das suas vizinhas, significa 
% que a adicao da ponte ponte(Pos1, Pos2) nao faz com que I e Vz deixem de ser vizinhas.
% ------------------------------------------------------------------------------

membereq(X, [H|_]) :-
    X == H.
membereq(X, [_|T]) :-
    membereq(X, T).

common_elements([H|_], L2) :-
    membereq(H, L2).
common_elements([_|T], L2) :-
    common_elements(T, L2).

same(L1, L2) :- maplist(=, L1, L2).

caminho_livre(_,_, Posicoes, ilha(_, Pos1), ilha(_, Pos2)):-

    posicoes_entre(Pos1, Pos2, Ls),
    not(common_elements(Posicoes, Ls));
    posicoes_entre(Pos1, Pos2, Lis),
    same(Posicoes, Lis).

% ------------------------------------------------------------------------------
% actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada).
% Pos1 e Pos2 sao as posicoes entre as quais irah ser adicionada uma ponte, 
% Posicoes eh a lista ordenada de posicoes entre Pos1 e Pos2
% Entrada eh uma entrada 
% Novo_estado eh o estado que se obtem de Estado apos a actualizacao das ilhas vizinhas.
% ------------------------------------------------------------------------------

getNovasVizinhas(_, _, [], New_Vizinhas, New_Vizinhas).
getNovasVizinhas(Posicoes, Ilha, [H|T], New_Vizinhas, Aux):-
        caminho_livre(_,_, Posicoes, Ilha, H)
        ->
        append(Aux, [H], New_Aux),
        getNovasVizinhas(Posicoes, Ilha, T, New_Vizinhas, New_Aux);
        getNovasVizinhas(Posicoes, Ilha, T, New_Vizinhas, Aux).
    

actualiza_vizinhas_entrada(_, _, Posicoes, [Ilha, Vizinhas, []], [Ilha, New_Vizinhas,[]]):-

    getNovasVizinhas(Posicoes, Ilha, Vizinhas, New_Vizinhas, []).

% ------------------------------------------------------------------------------
% actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado).
% Estado eh um estado
% Pos1 e Pos2 sao as posicoes entre as quais foi adicionada uma ponte
% Novo_estado eh o estado que se obtem de Estado apos a actualizacao das ilhas vizinhas.
% ------------------------------------------------------------------------------

actualiza_vizinhas_apos_pontes([], _, _, Novo_estado, Novo_estado).
actualiza_vizinhas_apos_pontes([H|T], Pos1, Pos2, Novo_estado, Aux):-
    posicoes_entre(Pos1,Pos2, Posicoes),
    actualiza_vizinhas_entrada(Pos1,Pos2, Posicoes, H, Estado_Atualizado),
    append(Aux, Estado_Atualizado, New_Aux),
    actualiza_vizinhas_apos_pontes(T, Pos1, Pos2, Novo_estado, New_Aux). 

actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado):-
    actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado, []).


% ------------------------------------------------------------------------------
%ilhas_terminadas(Estado, Ilhas_term).
%Estado eh um estado
%Ilhas_term eh a lista de ilhas que ja tem todas as pontes associadas.
% ------------------------------------------------------------------------------
equals(X, X).

terminada([ilha(N,_),_, Pontes]):-
    length(Pontes, Len),
    equals(N, Len).

ilhas_terminadas([], Ilhas_term, Ilhas_term).
ilhas_terminadas([[ilha(N,Pos),_, Pontes]|T], Ilhas_term, Aux):-
    terminada([ilha(N,Pos),_, Pontes])
    ->
    append(Aux, [ilha(N,Pos)], New_Aux),
    ilhas_terminadas(T, Ilhas_term, New_Aux);
    ilhas_terminadas(T, Ilhas_term, Aux).


ilhas_terminadas(Estado, Ilhas_term):-
    ilhas_terminadas(Estado, Ilhas_term, []).



% ------------------------------------------------------------------------------
%tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada)
%Ilhas_term eh uma lista de ilhas terminadas 
%Entrada eh uma entrada 
%Nova_entrada eh a entrada resultante de remover as ilhas de Ilhas_term, da lista de ilhas vizinhas de entrada.
% ------------------------------------------------------------------------------


remover( _, [], []).
remover( R, [R|T], T2) :- remover( R, T, T2).
remover( R, [H|T], [H|T2]) :- H \= R, remover( R, T, T2).

tira_ilhas_terminadas_entrada_aux([], New_Vizinhas, New_Vizinhas).
tira_ilhas_terminadas_entrada_aux([H|T], Vizinhas, New_Vizinhas):-
    remover(H, Vizinhas, Viz),
    tira_ilhas_terminadas_entrada_aux(T, Viz, New_Vizinhas).

tira_ilhas_terminadas_entrada(Ilhas_term, [Ilha, Vizinhas, Pontes], [Ilha, New_Vizinhas, Pontes]):-
    tira_ilhas_terminadas_entrada_aux(Ilhas_term, Vizinhas, New_Vizinhas).

% ------------------------------------------------------------------------------
%tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado). 
%Estado eh um estado
%Ilhas_term eh uma lista de ilhas terminadas
%Novo_estado eh o estado resultante de aplicar o predicado tira_ilhas_terminadas_entrada a cada uma das entradas de Estado.
% ------------------------------------------------------------------------------

tira_ilhas_terminadas_aux([], _, Novo_estado, Novo_estado).
tira_ilhas_terminadas_aux([H|T], Ilhas_term, Novo_estado, Aux):-
    tira_ilhas_terminadas_entrada(Ilhas_term, H, N),
    append(Aux, [N], New_Aux),
    tira_ilhas_terminadas_aux(T, Ilhas_term, Novo_estado, New_Aux).


tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado):-
tira_ilhas_terminadas_aux(Estado, Ilhas_term, Novo_estado, []).

% ------------------------------------------------------------------------------
% marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada). 
% Ilhas_term eh uma lista de ilhas terminadas
% Entrada eh uma entrada
% Nova_entrada eh a entrada obtida de Entrada com o estado de ilhas terminadas atualizado para "X".
% ------------------------------------------------------------------------------

marca_ilhas_terminadas_entrada(Ilhas_term, [ilha(N, Pos), Vizinhas, Pontes], [ilha('X', Pos), Vizinhas, Pontes]):- member(ilha(N, Pos), Ilhas_term).
marca_ilhas_terminadas_entrada(Ilhas_term, [Ilha, Vizinhas, Pontes], [Ilha, Vizinhas, Pontes]):- not(member(Ilha, Ilhas_term)).
  
% ------------------------------------------------------------------------------
% marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado)
% Estado eh um estado
% Ilhas_term eh uma lista de ilhas terminadas
% Novo_estado eh o estado resultante de aplicar o predicado marca_ilhas_terminadas_entrada a cada uma das entradas de Estado.
% ------------------------------------------------------------------------------

marca_ilhas_terminadas_aux([], _, Novo_estado, Novo_estado).
marca_ilhas_terminadas_aux([H|T], Ilhas_term, Novo_estado, Aux):-
    marca_ilhas_terminadas_entrada(Ilhas_term, H, N),
    append(Aux, [N], New_Aux),
    marca_ilhas_terminadas_aux(T, Ilhas_term, Novo_estado, New_Aux).


marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado):-
    marca_ilhas_terminadas_aux(Estado, Ilhas_term, Novo_estado, []).




% ------------------------------------------------------------------------------
% trata_ilhas_terminadas(Estado, Novo_estado).
% Estado eh um estado 
% Novo_estado eh o estado resultante de aplicar os predicados tira_ilhas_terminadas e marca_ilhas_terminadas a Estado.
% ------------------------------------------------------------------------------
   
trata_ilhas_terminadas(Estado, Novo_estado):-
    ilhas_terminadas(Estado, Ilhas_term),
    tira_ilhas_terminadas(Estado, Ilhas_term, N),
    marca_ilhas_terminadas(N, Ilhas_term, Novo_estado).

% ------------------------------------------------------------------------------
% junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado)
% Estado eh um estado 
% Ilha1 e Ilha2 sao 2 ilhas
% Novo_estado eh o estado que se obtem de Estado por adicao de Num_pontes pontes entre Ilha1 e Ilha2 .
% ------------------------------------------------------------------------------

/*TODO*/
