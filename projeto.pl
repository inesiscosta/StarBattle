:- use_module(library(clpfd)). % para poder usar transpose/2
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas
:- [puzzles]. % Ficheiro dado. A avaliação terá mais puzzles.
:- [codigoAuxiliar]. % Ficheiro dado. Não alterar.

/* Visualização */

% visualiza/1
% Imprime cada elemento da lista numa nova linha.
% Recebe: Lista
% Retorna: True se Lista é uma lista e a aplicação deste predicado permite
% escrever, por linha, cada elemento da lista, Lista.
visualiza([]).
visualiza([H|T]) :- writeln(H), visualiza(T).

% visualizaLinha/1
% Imprime cada elemento da linha com seu índice correspondente.
% Recebe: Lista
% Retorna: True se Lista é uma lista e a aplicação deste predicado permite
% escrever, por linha, cada elemento da lista (Lista)
% com o índice correspondente.
visualizaLinha(Linha) :- visualizaLinha(Linha, 1).
visualizaLinha([], _).
visualizaLinha([H|T], Index) :-
  % ~d - formata como inteiro, ~w fomato default, ~n new line.
  format("~d: ~w~n", [Index, H]),
  NextIndex is Index + 1,
  visualizaLinha(T, NextIndex).


/* Inserção */

% insereObjecto/3
% Insere um objeto numa célula do tabuleiro se a célula estiver vazia.
% Recebe: Coordenada (Linha, Coluna), Tabuleiro, Objeto
% Retorna: True se Tabuleiro é um tabuleiro que após a aplicação deste
% predicado passa a ter o objeto Objeto na coordenada (L, C), caso a célula
% contivesse originalmente uma variável.
insereObjecto((L, C), Tabuleiro, Objeto) :-
  (dentroLimites(Tabuleiro, (L, C)) -> 
    ehVar(Tabuleiro, (L, C)) -> 
    nth1(L, Tabuleiro, Linha),
    nth1(C, Linha, Objeto);
  true).

% insereVariosObjectos/3
% Insere vários objetos em várias coordenadas do tabuleiro.
% Recebe: Lista de Coordenadas (ListaCoords), Tabuleiro,
% Lista de Objetos(ListaObjs)
% Retorna: True se ListaCoords for uma lista de coordenadas, ListaObjs uma
% lista de objetos e Tabuleiro um tabuleiro que, após a aplicação deste
% predicado, passa a ter nas coordenadas de ListaCoords os objetos de
% ListaObjs. Falha se as listas tiverem tamanhos diferentes.
insereVariosObjectos(ListaCoords, Tabuleiro, ListaObjs) :-
  length(ListaCoords, LenCoords),
  length(ListaObjs, LenObjs),
  (LenCoords =\= LenObjs -> fail;
  % Usamos auxiliar de modo a verificar a condição
  % de falha logo no ínicio e apenas uma vez.
  insereVariosObjectosAux(ListaCoords, Tabuleiro, ListaObjs)).

% insereVariosObjectosAux/3 (Aux)
% Auxiliar para insereVariosObjectos/3 que assume que as listas têm o mesmo
% comprimento.
insereVariosObjectosAux([], _, []).
insereVariosObjectosAux([(L, C)|RemainingCoords], Tabuleiro,
[Obj|RemainingObjs]) :-
  insereObjecto((L, C), Tabuleiro, Obj),
  insereVariosObjectosAux(RemainingCoords, Tabuleiro, RemainingObjs).

% inserePontosVolta/2
% Insere pontos nas células adjacentes à coordenada dada.
% Recebe: Tabuleiro, Coordenada (L, C)
% Retorna: True se Tabuleiro é um tabuleiro que, após a aplicação do
% predicado, passa a ter pontos (p) nas células adjacentes às coordenadas
% (L, C) (cima, baixo, esquerda, direita e diagonais).
inserePontosVolta(Tabuleiro, (L, C)) :-
  adjacentes((L, C), Tabuleiro, Adjacentes),
  inserePontos(Tabuleiro, Adjacentes).

% adjacentes/3 (Aux)
% Calcula as coordenadas adjacentes a uma dada coordenada dentro dos limites
% do tabuleiro.
% Recebe: Coordenada (X, Y), Tabuleiro, Adjacentes (Lista Vazia)
% Retorna: True se Adjacentes é a lista de coordenadas adjacentes
% (cima, baixo, esquerda, direita e diagonais) dentro dos limites do Tabuleiro.
adjacentes((L, C), Tabuleiro, Adjacentes) :-
  findall((X, Y), (member((DX, DY),
    [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]),
    X is L + DX, Y is C + DY, dentroLimites(Tabuleiro, (X, Y))), Adjacentes).

% dentroLimites/2 (Aux)
% Verifica se uma coordenada está dentro dos limites do tabuleiro.
% Recebe: Tabuleiro (lista de listas), Coordenada (L, C)
% Retorna: True se a Coordenada (L, C) está dentro dos limites do Tabuleiro.
dentroLimites(Tabuleiro, (L, C)) :-
  length(Tabuleiro, NumLinhas),
  L > 0, L =< NumLinhas,
  nth1(1, Tabuleiro, Linha),
  length(Linha, NumColunas),
  C > 0, C =< NumColunas.

% inserePontos/2
% Insere pontos nas coordenadas dadas no tabuleiro.
% Recebe: Tabuleiro (Tabuleiro), Lista de Coordenadas (ListaCoord)
% Retorna: True se ListaCoord for uma lista de coordenadas e Tabuleiro
% um tabuleiro que, após a aplicação deste predicado, passa a ter nas
% coordenadas de ListaCoord pontos inseridos.
inserePontos(Tabuleiro, ListaCoord) :-
  maplist(inserePonto(Tabuleiro, p), ListaCoord).

% inserePonto/3 (Aux)
% Auxiliar para inserePontos/2 que insere um ponto numa célula do tabuleiro.
% Recebe: Tabuleiro, Ponto, (Linha, Coluna)
% Retorna: True se Tabuleiro é um tabuleiro que após a aplicação deste
% predicado passa a ter o ponto Ponto na coordenada (L, C), caso a célula
% contivesse originalmente uma variável.
inserePonto(Tabuleiro, Ponto, (L, C)) :-
  insereObjecto((L, C), Tabuleiro, Ponto).


/* Consultas */

% objectosEmCoordenadas/3
% Obtém os objetos nas coordenadas dadas do tabuleiro.
% Recebe: Lista de Coordenadas, Tabuleiro, Lista de Objetos
% Retorna: True se Coordenadas for uma lista de coordenadas, Tabuleiro um
% tabuleiro e Objetos uma lista de objetos que correspondem às coordenadas
% fornecidas.
objectosEmCoordenadas([], _, []).
objectosEmCoordenadas([(L, C)|Coords], Tabuleiro, [Obj|Objs]) :-
  dentroLimites(Tabuleiro, (L, C)),
  nth1(L, Tabuleiro, Linha),
  nth1(C, Linha, Obj),
  objectosEmCoordenadas(Coords, Tabuleiro, Objs).

% coordObjectos/5
% Obtém as coordenadas e o número de células que contêm o objeto dado.
% Recebe: Objecto, Tabuleiro, Lista de Coordenadas (ListaCoords)
% Retorna: Lista de Coordenadas dos Objetos (ListaCoordObjs) e
% Número de Objetos (NumObjectos).
coordObjectos(Objecto, Tabuleiro, ListaCoords, ListaCoordObjs, NumObjectos) :-
  findall((L, C), (member((L, C), ListaCoords),
    nth1(L, Tabuleiro, Linha), nth1(C, Linha, Celula),
    (Celula == Objecto; (var(Celula), var(Objecto)))), ListaCoordObjs),
  length(ListaCoordObjs, NumObjectos).

% coordenadasVars/2
% Obtém as coordenadas das células vazias do tabuleiro.
% Recebe: Tabuleiro (Tabuleiro)
% Retorna: Lista de Coordenadas das células vazias (ListaVars)
coordenadasVars(Tabuleiro, ListaVars) :-
  findall((L, C), (nth1(L, Tabuleiro, Linha), nth1(C, Linha, Celula),
    var(Celula)), ListaVars).


/* Estratégias */

% fechaListaCoordenadas/2
% Recebe: Tabuleiro, Lista de Coordenadas (ListaCoord)
% Retorna: O tabuleiro com uma das seguintes estratégias aplicadas:
% - h1: sempre que a linha, coluna ou região associada à lista de coordenadas
%       tiver duas estrelas, enche as restantes coordenadas de pontos;
% - h2: sempre que a linha, coluna ou região associada à lista de coordenadas
%       tiver uma única estrela e uma única posição livre, insere uma estrela
%       na posição livre e insere pontos à volta da estrela;
% - h3: sempre que a linha, coluna ou região associada à lista de coordenadas
%       não tiver nenhuma estrela e tiver duas únicas posições livres, insere
%       uma estrela em cada posição livre e insere pontos à volta de cada
%       estrela inserida.
fechaListaCoordenadas(Tabuleiro, ListaCoord) :-
  coordObjectos(e, Tabuleiro, ListaCoord, _, NumEstrelas),
  coordObjectos(_, Tabuleiro, ListaCoord, ListaVars, NumVars),
  (NumEstrelas =:= 2 ->
    inserePontos(Tabuleiro, ListaVars)
  ; NumEstrelas =:= 1, NumVars =:= 1 ->
    nth1(1, ListaVars, Coord),
    insereObjecto(Coord, Tabuleiro, e),
    inserePontosVolta(Tabuleiro, Coord)
  ; NumEstrelas =:= 0, NumVars =:= 2 ->
    insereVariosObjectos(ListaVars, Tabuleiro, [e, e]),
    maplist(inserePontosVolta(Tabuleiro), ListaVars)
  ; true).

% fecha/2
% Recebe: Tabuleiro, Lista de Listas de Coordenadas (ListaListasCoord)
% Retorna: Aplica o predicado fechaListaCoordenadas a cada
% lista de ListaListasCoord.
fecha(Tabuleiro, ListaListasCoord) :-
  maplist(fechaListaCoordenadas(Tabuleiro), ListaListasCoord).

% encontraSequencia/4
% Recebe: Tabuleiro, tamanho da sequência (N),
% Lista de Coordenadas (ListaCoord), Sequencia (Seq)
% Retorna: True se Seq for uma sequência de tamanho N
% de coordenadas de pontos ou variáveis.
encontraSequencia(Tabuleiro, N, ListaCoords, Seq) :-
  maplist(ehVarOuPonto(Tabuleiro), ListaCoords),
  include(ehVar(Tabuleiro), ListaCoords, Seq),
  ehSubSequencia(ListaCoords, Seq), !,
  length(Seq, N).

% ehVar/2 (Aux)
% Recebe: Tabuleiro, Coordenada
% Devolve: True se na coordenada dada estiver uma variavel.
ehVar(Tabuleiro, (L,C)) :-
  nth1(L, Tabuleiro, Linha),
  nth1(C, Linha, Celula),
  var(Celula).

% ehVarOuPonto/2 (Aux)
% Recebe: Tabuleiro, Coordenada
% Devolve: True se na coordenada dada estiver uma variavel ou um ponto.
ehVarOuPonto(Tabuleiro, (L, C)) :-
  dentroLimites(Tabuleiro, (L, C)),
  nth1(L, Tabuleiro, Linha),
  nth1(C, Linha, Celula),
  (var(Celula); Celula == p).

% ehSubSequencia/2
% Recebe: Lista, SubLista
% Retorna: True se SubLista for uma subsequência de Lista.
ehSubSequencia(_, []).
ehSubSequencia([], _) :- false.
ehSubSequencia([Head|Tail], [Head|SubTail]) :-
  ehSubSequencia(Tail, SubTail).
ehSubSequencia([_|Tail], SubList) :-
  ehSubSequencia(Tail, SubList).

% aplicaPadraoI/2
% Aplica o padrão I no tabuleiro, inserindo estrelas no inicio e fim da lista e
% pontos nas coordenadas à volta das estrelas colocadas.
% Recebe: Tabuleiro, Lista de Coordenadas de tamanho 3
% [(L1, C1), (L2, C2), (L3, C3)]
% Retorna: True se Tabuleiro passa a ter estrelas nas coordenadas
% (L1, C1) e (L3, C3) e pontos nas coordenadas adjacentes.
aplicaPadraoI(Tabuleiro, [(L1, C1), (L2, C2), (L3, C3)]) :-
  encontraSequencia(Tabuleiro, 3, [(L1, C1), (L2, C2), (L3, C3)], _),
  insereVariosObjectos([(L1, C1), (L3, C3)], Tabuleiro, [e, e]),
  maplist(inserePontosVolta(Tabuleiro), [(L1, C1), (L3, C3)]).

% aplicaPadroes/2
% Aplica os padrões I e T no tabuleiro.
% Recebe: Tabuleiro, Lista de Listas de Coordenadas (ListaListaCoords)
% Retorna: True se Tabuleiro for um tabuleiro e ListaListaCoords for uma lista
% de listas de coordenadas, após a aplicação deste predicado ter-se-ão
% encontrado sequências de tamanho 3 e aplicado o aplicaPadraoI/2, ou então
% ter-se-ão encontrado sequências de tamanho 4 e aplicado o aplicaPadraoT/2.
aplicaPadroes(Tabuleiro, ListaListaCoords) :-
  maplist(aplicaPadroesAux(Tabuleiro), ListaListaCoords).

% aplicaPadroesAux/2 (Aux)
% Auxiliar para aplicaPadroes/2 que aplica os padrões I e T no tabuleiro.
% Recebe: Tabuleiro, Lista de Coordenadas (ListaCoord)
% Retorna: True se Tabuleiro for um tabuleiro e ListaCoord for umalista de
% coordenadas, após a aplicação deste predicado ter-se-ão encontrado
% sequências de tamanho 3 e aplicado o aplicaPadraoI/2, ou então ter-se-ão
% encontrado sequências de tamanho 4 e aplicado o aplicaPadraoT/2.
aplicaPadroesAux(Tabuleiro, ListaCoord) :-
  (encontraSequencia(Tabuleiro, 3, ListaCoord, Seq3) ->
  aplicaPadraoI(Tabuleiro, Seq3);
  encontraSequencia(Tabuleiro, 4, ListaCoord, Seq4) ->
  aplicaPadraoT(Tabuleiro, Seq4);
  true).


/* Apoteose Final */

% resolve/2
% Resolve o puzzle StarBattle para o caso das duas estrelas.
% Recebe: Estruturas (Regiões do Tabuleiro), Tabuleiro
% Retorna: Tabuleiro resolvido ou o mais resolvido possível deterministicamente.
resolve(Estruturas, Tabuleiro) :- 
  copy_term(Tabuleiro, CopiaTabuleiro),
  coordRegioes(Estruturas, CoordRegioes),
  resolve(Estruturas, Tabuleiro, CopiaTabuleiro, CoordRegioes).

resolve(Estruturas, Tabuleiro, PrevTabuleiro, CoordRegioes) :-
  coordenadasVars(Tabuleiro, ListaVars),
  \+ length(ListaVars, 0),
  copy_term(Tabuleiro, NewPrevTabuleiro),
  aplicaPadroes(Tabuleiro, CoordRegioes),
  fecha(Tabuleiro, CoordRegioes),
  \+ Tabuleiro == PrevTabuleiro, 
  resolve(Estruturas, Tabuleiro, NewPrevTabuleiro).

resolve(_,_,_).
