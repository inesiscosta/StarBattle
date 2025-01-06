% Inês Isabel Santos Costa, ist1110632
:- use_module(library(clpfd)). % para poder usar transpose/2
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas
:- [puzzles]. % Ficheiro dado. A avaliação terá mais puzzles.
:- [codigoAuxiliar]. % Ficheiro dado. Não alterar.
% Segue-se o código


% Don't forget to add verifications for eh_coordenada and eh_tabuleiro or use structs.

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
  (dentroLimites(Tabuleiro, (L, C)) -> 
    nth1(L, Tabuleiro, Linha),
    nth1(C, Linha, Celula),
    (var(Celula) -> Celula = Objeto; true);
  true).

% insereVariosObjectos/3
% Insere vários objetos em várias coordenadas do tabuleiro.
% Recebe: Lista de Coordenadas (ListaCoords), Tabuleiro, Lista de Objetos(ListaObjs)
% Retorna: True se ListaCoords for uma lista de coordenadas, ListaObjs uma lista de objetos
% e Tabuleiro um tabuleiro que, após a aplicação deste predicado, passa a ter nas coordenadas
% de ListaCoords os objetos de ListaObjs. Falha se as listas tiverem tamanhos diferentes.
insereVariosObjectos(ListaCoords, Tabuleiro, ListaObjs) :-
  length(ListaCoords, LenCoords),
  length(ListaObjs, LenObjs),
  (LenCoords =\= LenObjs -> fail;
  % Usamos auxiliar de modo a verificar a condição de falha logo no ínicio e apenas uma vez.
  insereVariosObjectosAux(ListaCoords, Tabuleiro, ListaObjs)).

% insereVariosObjectosAux/3 (Aux)
% Auxiliar para insereVariosObjectos/3 que assume que as listas têm o mesmo comprimento.
insereVariosObjectosAux([], _, []).
insereVariosObjectosAux([(L, C)|RemainingCoords], Tabuleiro, [Obj|RemainingObjs]) :-
  insereObjecto((L, C), Tabuleiro, Obj),
  insereVariosObjectosAux(RemainingCoords, Tabuleiro, RemainingObjs).

% inserePontosVolta/2
% Insere pontos nas células adjacentes à coordenada dada.
% Recebe: Tabuleiro, Coordenada (L, C)
% Retorna: True se Tabuleiro é um tabuleiro que, após a aplicação do predicado, passa a ter
% pontos (p) nas células adjacentes às coordenadas (L, C) (cima, baixo, esquerda, direita e diagonais).
inserePontosVolta(Tabuleiro, (L, C)) :-
  adjacentes((L, C), Tabuleiro, Adjacentes),
  inserePontos(Tabuleiro, Adjacentes).

% adjacentes/3 (Aux)
% Calcula as coordenadas adjacentes a uma dada coordenada dentro dos limites do tabuleiro.
% Recebe: Coordenada (X, Y), Tabuleiro, Adjacentes (Lista Vazia)
% Retorna: True se Adjacentes é a lista de coordenadas adjacentes (cima, baixo, esquerda, direita e diagonais) dentro dos limites do Tabuleiro.
adjacentes((L, C), Tabuleiro, Adjacentes) :-
  findall((X, Y), 
    (member((DX, DY), [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]), 
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
% Retorna: True se ListaCoord for uma lista de coordenadas e Tabuleiro um tabuleiro que,
% após a aplicação deste predicado, passa a ter nas coordenadas de ListaCoord pontos inseridos.
inserePontos(Tabuleiro, ListaCoord) :-
  maplist(inserePontosAux(Tabuleiro, p), ListaCoord).

% inserePontosAux/3 (Aux)
% Auxiliar para inserePontos/2 que insere um ponto numa célula do tabuleiro.
% Recebe: Tabuleiro, Ponto, (Linha, Coluna)
% Retorna: True se Tabuleiro é um tabuleiro que após a aplicação deste predicado
% passa a ter o ponto Ponto na coordenada (L, C), caso a célula
% contivesse originalmente uma variável.
inserePontosAux(Tabuleiro, Ponto, (L, C)) :-
  insereObjecto((L, C), Tabuleiro, Ponto).


/* Consultas */

% objectosEmCoordenadas/3
% Obtém os objetos nas coordenadas dadas do tabuleiro.
% Recebe: Lista de Coordenadas (Coordenadas), Tabuleiro, Lista de Objetos (Objetos)
% Retorna: True se Coordenadas for uma lista de coordenadas, Tabuleiro um tabuleiro
% e Objetos uma lista de objetos que correspondem às coordenadas fornecidas.
objectosEmCoordenadas([], _, []).
objectosEmCoordenadas([(L, C)|Coords], Tabuleiro, [Obj|Objs]) :-
  dentroLimites(Tabuleiro, (L, C)),
  nth1(L, Tabuleiro, Linha),
  nth1(C, Linha, Obj),
  objectosEmCoordenadas(Coords, Tabuleiro, Objs).

% coordObjectos/5
% Obtém as coordenadas e o número de células que contêm o objeto dado.
% Recebe: Objecto, Tabuleiro, Lista de Coordenadas (ListaCoords)
% Retorna: Lista de Coordenadas dos Objetos (ListaCoordObjs) e Número de Objetos (NumObjectos)
coordObjectos(Objecto, Tabuleiro, ListaCoords, ListaCoordObjs, NumObjectos) :-
  findall((L, C), (member((L, C), ListaCoords),
    nth1(L, Tabuleiro, Linha), nth1(C, Linha, Celula),
    (Celula == Objecto; (var(Objecto), var(Celula)))
  ), ListaCoordObjs),
  length(ListaCoordObjs, NumObjectos).

% coordenadasVars/2
% Obtém as coordenadas das células vazias do tabuleiro.
% Recebe: Tabuleiro (Tabuleiro)
% Retorna: Lista de Coordenadas das células vazias (ListaVars)
coordenadasVars(Tabuleiro, ListaVars) :-
  findall((L, C), (nth1(L, Tabuleiro, Linha), nth1(C, Linha, Celula), var(Celula)), ListaVars).
