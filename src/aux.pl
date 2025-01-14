:- use_module(library(clpfd)).

clearBoard([], []) :-!.
clearBoard([L | T], [L1 | T1]) :-
  clearLine(L, L1),
  clearBoard(T, T1).

clearLine([], []) :- !.
clearLine([H | T], [v | L]) :- var(H), !, clearLine(T, L).
clearLine([H | T], [H | L]) :- clearLine(T, L).

lineCoords(Num, LineCoords) :-  
  lineCoords(Num, LineCoords, 1, []). 
lineCoords(Num, LineCoords, N, LineCoords) :- N > Num, !. 
lineCoords(Num, LineCoords, N, Aux) :-  
  findall((N, C), between(1, Num, C), LineCoordN),
  append(Aux, [LineCoordN], NewAux),
  NewN is N + 1,
  lineCoords(Num, LineCoords, NewN, NewAux).

columnCoords(Num, ColumnCoords) :- 
  lineCoords(Num, LineCoords),
  transpose(LineCoords, ColumnCoords).

coordOneRegion(Structures, NumRegion, CoordRegion) :-
  length(Structures, Size),
  findall((L, C), (between(1, Size, L),
    between(1, Size, C), 
    nth1(L, Structures, Linha), 
    nth1(C, Linha, NumRegion)), AuxList),
  sort(AuxList, CoordRegion).

coordRegions(Structures, CoordRegions) :-
  length(Structures, Num),
  coordRegions(Structures, 1, Num, [], CoordRegions).
coordRegions(_, Acc, Num, CoordRegions, CoordRegions) :- 
  Acc > Num, !.
coordRegions(Structures, Acc, Num, ListaAcc, CoordRegions) :-
  coordOneRegion(Structures, Acc, CoordRegion),
  append(ListaAcc, [CoordRegion], ListaAcc1),
  NovoAcc is Acc + 1,
  coordRegions(Structures, NovoAcc, Num, ListaAcc1, CoordRegions). 
 
allCoords(Structures, AllCoords) :-
  length(Structures, Num),
  lineCoords(Num, LineCoords),
  columnCoords(Num, ColumnCoords),
  coordRegions(Structures, RegionCoords),
  append(LineCoords, ColumnCoords, AuxCoords),
  append(AuxCoords, RegionCoords, AllCoords).
