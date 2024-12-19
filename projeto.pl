% Inês Isabel Santos Costa, ist1110632
:- use_module(library(clpfd)). % para poder usar transpose/2
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas
:- [puzzles]. % Ficheiro dado. A avaliação terá mais puzzles.
:- [codigoAuxiliar]. % Ficheiro dado. Não alterar.
% Segue-se o código

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
% escrever, por linha, cada elemento da lista (Lista) com o índice correspondente.
visualizaLinha(Linha) :- visualizaLinha(Linha, 1).
visualizaLinha([], _).
visualizaLinha([H|T], Index) :-
  format("~d: ~w~n", [Index, H]), % ~d - formata como inteiro, ~w fomato default, ~n new line
  NextIndex is Index + 1,
  visualizaLinha(T, NextIndex).


/* Inserção */

% insereObjecto/3
% Insere um objeto numa célula do tabuleiro se a célula estiver vazia.
% Recebe: Coordenada (Linha, Coluna), Tabuleiro, Objeto
% Retorna: True se Tabuleiro é um tabuleiro que após a aplicação deste predicado
% passa a ter o objeto Objeto na coordenada (L, C), caso a célula
% contivesse originalmente uma variável.
insereObjecto((L, C), Tabuleiro, Objeto) :-
  nth1(L, Tabuleiro, Linha),
  nth1(C, Linha, Celula),
  ( var(Celula) -> Celula = Objeto ; true ), !.
insereObjecto(_, _, _).

% insereVariosObjectos/3
% Insere vários objetos em várias coordenadas do tabuleiro.
% Recebe: Lista de Coordenadas (ListaCoords), Tabuleiro, Lista de Objetos(ListaObjs)
% Retorna: True se ListaCoords for uma lista de coordenadas, ListaObjs uma lista de objetos
% e Tabuleiro um tabuleiro que, após a aplicação deste predicado, passa a ter nas coordenadas
% de ListaCoords os objetos de ListaObjs.
insereVariosObjectos([], _, []).
insereVariosObjectos([(L, C)|Coords], Tabuleiro, [Obj|Objs]) :-
  insereObjecto((L, C), Tabuleiro, Obj),
  insereVariosObjectos(Coords, Tabuleiro, Objs).

% inserePontosVolta/2
% Insere pontos nas células adjacentes à coordenada dada.
% Recebe: Tabuleiro, Coordenada (L, C)
% Retorna: True se Tabuleiro é um tabuleiro que, após a aplicação do predicado, passa a ter
% pontos (p) nas células adjacentes às coordenadas (L, C) (cima, baixo, esquerda, direita e diagonais).
inserePontosVolta(Tabuleiro, (L, C)) :-
  adjacentes((L, C), Adjacentes),
  include(dentro_limites(Tabuleiro), Adjacentes, AdjacentesValidos),
  inserePontos(Tabuleiro, AdjacentesValidos).

% adjacentes/2 (Aux)
% Calcula as coordenadas adjacentes a uma dada coordenada.
% Recebe: Coordenada (X, Y)
% Retorna: True se Adjacentes é a lista de coordenadas adjacentes (cima, baixo, esquerda, direita e diagonais).
adjacentes((L, C), Adjacentes) :-
  findall((X, Y), (member((DX, DY), [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]), X #= L + DX, Y #= C + DY), Adjacentes),
  Adjacentes \= [].

% dentro_limites/2 (Aux)
% Verifica se uma coordenada está dentro dos limites do tabuleiro.
% Recebe: Tabuleiro (lista de listas), Coordenada (L, C)
% Retorna: True se a Coordenada (L, C) está dentro dos limites do Tabuleiro.
dentro_limites(Tabuleiro, (L, C)) :-
  length(Tabuleiro, NumLinhas),
  nth1(1, Tabuleiro, Linha),
  length(Linha, NumColunas),
  L > 0, L =< NumLinhas,
  C > 0, C =< NumColunas.

% inserePontos/2
% Insere pontos nas coordenadas dadas no tabuleiro.
% Recebe: Tabuleiro (Tabuleiro), Lista de Coordenadas (ListaCoord)
% Retorna: True se ListaCoord for uma lista de coordenadas e Tabuleiro um tabuleiro que,
% após a aplicação deste predicado, passa a ter nas coordenadas de ListaCoord pontos inseridos.
inserePontos(Tabuleiro, ListaCoord) :-
  maplist(inserePonto(Tabuleiro), ListaCoord).

% inserePonto/2 (Aux)
% Insere um ponto numa célula do tabuleiro se a célula estiver vazia.
% Recebe: Tabuleiro (Tabuleiro), Coordenada (Coordenada)
% Retorna: True se Coordenada for uma coordenada válida e Tabuleiro um tabuleiro que,
% após a aplicação deste predicado, passa a ter na coordenada especificada um ponto inserido,
% caso a célula esteja vazia.
inserePonto(Tabuleiro, (L, C)) :-
  nth1(L, Tabuleiro, Linha),
  nth1(C, Linha, Celula),
  ( var(Celula) -> Celula = p ; true ).


/* Consultas */

% objectosEmCoordenadas/3
% Obtém os objetos nas coordenadas dadas do tabuleiro.
% Recebe: Lista de Coordenadas (Coordenadas), Tabuleiro, Lista de Objetos (Objetos)
% Retorna: True se Coordenadas for uma lista de coordenadas, Tabuleiro um tabuleiro
% e Objetos uma lista de objetos que correspondem às coordenadas fornecidas.
objectosEmCoordenadas([], _, []).
objectosEmCoordenadas([(L, C)|Coords], Tabuleiro, [Obj|Objs]) :-
  dentro_limites(Tabuleiro, (L, C)),
  nth1(L, Tabuleiro, Linha),
  nth1(C, Linha, Obj),
  objectosEmCoordenadas(Coords, Tabuleiro, Objs).

% coordObjectos/5
% Obtém as coordenadas e o número de células que contêm o objeto dado.
% Recebe: Objecto, Tabuleiro, Lista de Coordenadas (ListaCoords)
% Retorna: Lista de Coordenadas dos Objetos (ListaCoordObjs) e Número de Objetos (NumObjectos)
coordObjectos(Objecto, Tabuleiro, ListaCoords, ListaCoordObjs, NumObjectos) :-
  findall((L, C), (
    member((L, C), ListaCoords),
    nth1(L, Tabuleiro, Linha),
    nth1(C, Linha, Celula),
    (Celula == Objecto ; (var(Objecto), var(Celula)))
  ), ListaCoordObjs),
  length(ListaCoordObjs, NumObjectos).

% coordenadasVars/2
% Obtém as coordenadas das células vazias do tabuleiro.
% Recebe: Tabuleiro (Tabuleiro)
% Retorna: Lista de Coordenadas das células vazias (ListaVars)
coordenadasVars(Tabuleiro, ListaVars) :-
  findall((L, C), (nth1(L, Tabuleiro, Linha), nth1(C, Linha, Celula), var(Celula)), ListaVars).

