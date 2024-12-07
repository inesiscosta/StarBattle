limpaTabuleiro([], []) :-!.
limpaTabuleiro([L | T], [L1 | T1]) :- limpaLinha(L, L1), limpaTabuleiro(T, T1).

limpaLinha([], []) :- !.
limpaLinha([H | T], [v | L]) :- var(H), !, limpaLinha(T, L).
limpaLinha([H | T], [H | L]) :- limpaLinha(T, L).

%--------------------------
%
%  Gera (a matriz com as linhas esta definida)
%
%--------------------------

/* 
Gera matriz com todas as coordenadas das linhas
?- coordLinhas(4, CoordLinhas).
CoordLinhas = [[(1,1),(1,2),(1,3),(1,4)],[(2,1),(2,2),(2,3),(2,4)],
[(3,1),(3,2),(3,3),(3,4)],[(4,1),(4,2),(4,3),(4,4)]].
*/

coordLinhas(Num, CoordLinhas) :-  
    coordLinhas(Num, CoordLinhas, 1, []). 

coordLinhas(Num, CoordLinhas, N, CoordLinhas) :- N > Num, !.    
coordLinhas(Num, CoordLinhas, N, Aux) :-  
    findall((N, C), between(1, Num, C), CoordLinhaN),
    append(Aux, [CoordLinhaN], NovoAux),
    NovoN is N + 1,
    coordLinhas(Num, CoordLinhas, NovoN, NovoAux).
                     
/* 
Gera matriz com as coordenadas das colunas
?- coordColunas(4, CoordColunas).
CoordColunas = [[(1,1),(2,1),(3,1),(4,1)],
[(1,2),(2,2),(3,2),(4,2)],
[(1,3),(2,3),(3,3),(4,3)],
[(1,4),(2,4),(3,4),(4,4)]].
*/

coordColunas(Num, CoordColunas) :- 
  coordLinhas(Num, CoordLinhas),
  transpose(CoordLinhas, CoordColunas).
      
/*
Coordenadas de uma regicao ordenadas por linha e coluna (usar sort)
*/
coordUmaRegiao(Estruturas, NumRegiao, CoordRegiao) :-
   length(Estruturas, Size),
   findall((L, C), (between(1, Size, L),
                             between(1, Size, C), 
                             nth1(L, Estruturas, Linha), 
                             nth1(C, Linha, NumRegiao)), ListaAux),
   sort(ListaAux, CoordRegiao).

/*
Gera matriz com as coordenadas das regioes 
:- tabuleiro(1, T), regiao(T, 3, Lista).
...
Lista = [(2,2),(2,3),(3,1),(3,2),(3,3),(3,4),
(4,1),(4,2),(4,3),(4,4),(4,5),
(5,2),(5,3),(5,4),(5,5),(5,6),(5,7),
(6,3),(6,4)].
*/       
coordRegioes(Estruturas, CoordRegioes) :-
   length(Estruturas, Num),
   coordRegioes(Estruturas, 1, Num, [], CoordRegioes).

coordRegioes(_, Acc, Num, CoordRegioes, CoordRegioes) :- Acc > Num, !.

coordRegioes(Estruturas, Acc, Num, ListaAcc, CoordRegioes) :-
   coordUmaRegiao(Estruturas, Acc, CoordRegiao),
   append(ListaAcc, [CoordRegiao], ListaAcc1),
   NovoAcc is Acc + 1,
   coordRegioes(Estruturas, NovoAcc, Num, ListaAcc1, CoordRegioes). 

/*
Gera matriz com todas as coordenadas: linhas, colunas e regioes
*/     
coordTodas(Estruturas, CoordTodas) :-
    length(Estruturas, Num),
    
    coordLinhas(Num, CoordLinhas),
    coordColunas(Num, CoordColunas),
    coordRegioes(Estruturas, CoordRegioes),
    append(CoordLinhas, CoordColunas, CoordAux),
    append(CoordAux, CoordRegioes, CoordTodas).


/*
Padrao T - dado (aparece no código)
*
. .
*

  *
. .
  *
  
* . *
  .
  
  . 
* . *
*/ 
aplicaPadraoT(Tabuleiro, [(L1, C1), (L2, C2), (L3, C3), (L4, C4)]) :-
  L2 is L1 + 1, L3 is L2, L4 is L3 + 1, 
  (C2 is C1, C3 is C1 + 1, C4 is C1; C2 is C1 - 1, C3 is C1, C4 is C1), !,
  insereVariosObjectos([(L1, C1), (L4, C4)], Tabuleiro, [e, e]),
   inserePontosVolta(Tabuleiro, (L1, C1)),
   inserePontosVolta(Tabuleiro, (L4, C4)).

aplicaPadraoT(Tabuleiro, [(L1, C1), (L2, C2), (L3, C3), (L4, C4)]) :-
  L2 is L1, L3 is L2, L4 is L1 + 1, 
  C2 is C1 + 1, C3 is C2 + 1, C4 is C2, !,
  insereVariosObjectos([(L1, C1), (L3, C3)], Tabuleiro, [e, e]),
  inserePontosVolta(Tabuleiro, (L1, C1)),
  inserePontosVolta(Tabuleiro, (L3, C3)).

aplicaPadraoT(Tabuleiro, [(L1, C1), (L2, C2), (L3, C3), (L4, C4)]) :-
  L2 is L1 + 1, L3 is L2, L4 is L1, 
  C2 is C1 - 1, C3 is C2 + 1, C4 is C3 + 1, !,
  insereVariosObjectos([(L2, C2), (L4, C4)], Tabuleiro, [e, e]),
  inserePontosVolta(Tabuleiro, (L2, C2)),
  inserePontosVolta(Tabuleiro, (L4, C4)).
  