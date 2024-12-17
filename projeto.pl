% Inês Isabel Santos Costa, ist1110632
:- use_module(library(clpfd)). % para poder usar transpose/2
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas
:- [puzzles]. % Ficheiro dado. A avaliação terá mais puzzles.
:- [codigoAuxiliar]. % Ficheiro dado. Não alterar.
% Segue-se o código

% Visualização
visualiza([]).
visualiza([H|T]) :- writeln(H), visualiza(T).

visualizaLinha(Linha) :- visualizaLinha(Linha, 1).
visualizaLinha([], _).
visualizaLinha([H|T], Index) :-
    format("~d: ~w~n", [Index, H]), % ~d - formata como inteiro, ~w fomato default, ~n new line
    NextIndex is Index + 1,
    visualizaLinha(T, NextIndex).
