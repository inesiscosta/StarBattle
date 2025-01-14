:- set_prolog_flag(answer_write_options,[max_depth(0)]).
:- [aux].
:- [tests/puzzles].

/* Visualization */

% visualize/1
% Prints each element of the list on a new line.
% Receives: List
% Returns: True if List is a list and applying this predicate allows
% writing each element of the list, List, line by line.
visualize([]).
visualize([H|T]) :- writeln(H), visualize(T).

% visualizeLine/1
% Prints each element of the line with its corresponding index.
% Receives: List
% Returns: True if List is a list and applying this predicate allows writing
% each element of the list (List) line by line with the corresponding index.
visualizeLine(Line) :- visualizeLine(Line, 1).
visualizeLine([], _).
visualizeLine([H|T], Index) :-
  % ~d - formats as integer, ~w default format, ~n new line.
  format("~d: ~w~n", [Index, H]),
  NextIndex is Index + 1,
  visualizeLine(T, NextIndex).


/* Insertion */

% insertObject/3
% Inserts an object into a cell of the board if the cell is empty.
% Receives: Coordinate (Row, Column), Board, Object
% Returns: True if Board is a board that after applying this
% predicate has the object Object at coordinate (R, C), if the cell
% originally contained a variable.
insertObject((R, C), Board, Object) :-
  % If the coordinate is out of the board, the predicate does not fail.
  (\+ withinLimits(Board, (R, C)) -> true;
  nth1(R, Board, Row),
  nth1(C, Row, Cell),
  (var(Cell) -> Cell = Object; true)).

% insertMultipleObjects/3
% Inserts multiple objects into multiple coordinates of the board.
% Receives: List of Coordinates (CoordList), Board,
% List of Objects (ObjList)
% Returns: True if CoordList is a list of coordinates, ObjList a
% list of objects and Board a board that, after applying this
% predicate, has the objects from ObjList at the coordinates of CoordList.
% Fails if the lists have different lengths.
insertMultipleObjects(CoordList, Board, ObjList) :-
  length(CoordList, LenCoords),
  length(ObjList, LenObjs),
  (LenCoords =\= LenObjs -> fail;
  % We use an auxiliary to check the failure condition
  % at the beginning and only once.
  insertMultipleObjectsAux(CoordList, Board, ObjList)).

% insertMultipleObjectsAux/3 (Aux)
% Auxiliary for insertMultipleObjects/3 that assumes
% the lists have the same length.
insertMultipleObjectsAux([], _, []).
insertMultipleObjectsAux([(R, C)|Coords], Board, [Obj|Objs]) :-
  insertObject((R, C), Board, Obj),
  insertMultipleObjectsAux(Coords, Board, Objs).

% insertPointsAround/2
% Inserts points in the cells adjacent to the given coordinate.
% Receives: Board, Coordinate (R, C)
% Returns: True if Board is a board that, after applying this
% predicate, has points (p) in the cells adjacent to the coordinates
% (R, C) (up, down, left, right, and diagonals).
insertPointsAround(Board, (R, C)) :-
  adjacent((R, C), Board, Adjacent),
  insertPoints(Board, Adjacent).

% adjacent/3 (Aux)
% Calculates the coordinates adjacent to a given coordinate within the limits
% of the board.
% Receives: Coordinate (X, Y), Board, Adjacent (Empty List)
% Returns: True if Adjacent is the list of adjacent coordinates
% (up, down, left, right, and diagonals) within the limits of the Board.
adjacent((R, C), Board, Adjacent) :-
  findall((X, Y), (member((DX, DY),
    [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]),
    X is R + DX, Y is C + DY, withinLimits(Board, (X, Y))), Adjacent).

% withinLimits/2 (Aux)
% Checks if a coordinate is within the limits of the board.
% Receives: Board (list of lists), Coordinate (R, C)
% Returns: True if the Coordinate (R, C) is within the limits of the Board.
withinLimits(Board, (R, C)) :-
  length(Board, NumRows),
  R > 0, R =< NumRows,
  nth1(1, Board, Row),
  length(Row, NumCols),
  C > 0, C =< NumCols.

% insertPoints/2
% Inserts points at the given coordinates on the board.
% Receives: Board (Board), List of Coordinates (CoordList)
% Returns: True if CoordList is a list of coordinates and Board
% a board that, after applying this predicate, has points inserted at
% the coordinates of CoordList.
insertPoints(Board, CoordList) :-
  maplist(insertPoint(Board), CoordList).

% insertPoint/3 (Aux)
% Auxiliary for insertPoints/2 that inserts a point into a cell of the board.
% Receives: Board, Coord
% Returns: True if Board is a board that after applying this
% predicate has a point at the coordinate (Coord), if the cell
% originally contained a variable.
insertPoint(Board, Coord) :-
  insertObject(Coord, Board, p).


/* Queries */

% objectsInCoordinates/3
% Gets the objects at the given coordinates of the board.
% Receives: List of Coordinates, Board, List of Objects
% Returns: True if Coordinates is a list of coordinates, Board a
% board and Objects a list of objects that correspond to the given coordinates.
objectsInCoordinates([], _, []).
objectsInCoordinates([(R, C)|Coords], Board, [Obj|Objs]) :-
  withinLimits(Board, (R, C)),
  nth1(R, Board, Row),
  nth1(C, Row, Obj),
  objectsInCoordinates(Coords, Board, Objs).

% coordObjects/5
% Gets the coordinates and the number of cells containing the given object.
% Receives: Object, Board, List of Coordinates (CoordList)
% Returns: List of Coordinates of the Objects (CoordObjList) and
% Number of Objects (NumObjects).
coordObjects(Object, Board, CoordList, CoordObjList, NumObjects) :-
  findall((R, C), (member((R, C), CoordList),
    nth1(R, Board, Row), nth1(C, Row, Cell),
    (Cell == Object; (var(Cell), var(Object)))), UnsortedCoordObjList),
  sort(UnsortedCoordObjList, CoordObjList),
  length(CoordObjList, NumObjects).

% coordinatesVars/2
% Gets the coordinates of the empty cells of the board and
% sorts them by row and column.
% Receives: Board (Board)
% Returns: List of Coordinates of the empty cells (VarList)
coordinatesVars(Board, VarList) :-
  findall((R, C), (nth1(R, Board, Row), nth1(C, Row, Cell),
    var(Cell)), UnsortedVars),
  sort(UnsortedVars, VarList).


/* Strategies */

% closeCoordList/2
% Receives: Board, List of Coordinates (CoordList)
% Returns: The board with one of the following strategies applied:
% - h1: whenever the row, column, or region associated with the list of
%       coordinates has two stars, fill the remaining coordinates with points;
% - h2: whenever the row, column, or region associated with the list of
%       coordinates has one star and one free position, insert a star in the
%       free position and insert points around the star;
% - h3: whenever the row, column, or region associated with the list of
%       coordinates has no stars and two free positions, insert a star in
%       each free position and insert points around each inserted star.
closeCoordList(Board, CoordList) :-
  coordObjects(s, Board, CoordList, _, NumStars),
  coordObjects(_, Board, CoordList, VarsList, NumVars),
  (NumStars == 2 ->
    insertPoints(Board, VarsList)
  ; NumStars == 1, NumVars == 1 ->
    nth1(1, VarsList, Coord),
    insertObject(Coord, Board, s),
    insertPointsAround(Board, Coord)
  ; NumStars == 0, NumVars == 2 ->
    insertMultipleObjects(VarsList, Board, [s, s]),
    maplist(insertPointsAround(Board), VarsList)
  ; true).

% closeLists/2
% Receives: Board, List of Lists of Coordinates (CoordLists)
% Returns: Applies the predicate closeCoordList to each
% list in CoordLists.
closeLists(Board, CoordLists) :-
  maplist(closeCoordList(Board), CoordLists).

% findSequence/4
% Receives: Board, sequence length (N),
% List of Coordinates (CoordList), Sequence (Seq)
% Returns: True if Seq is a sequence of length N
% of variable coordinates.
findSequence(Board, N, CoordList, Seq) :-
  coordObjects(s, Board, CoordList, _, NumStars),
  NumStars == 0,
  coordinatesVars(Board, VarsList),
  % The order is important, CoordList must come first in the intersection
  % for Seq to maintain the order of CoordList.
  intersection(CoordList, VarsList, Seq),
  length(Seq, N),
  % Checks if Seq is a continuous subsequence of CoordList.
  append(_, OtherList, CoordList),
  append(Seq, _, OtherList), !.

% applyPatternI/2
% Applies the I pattern on the board, inserting stars at the beginning and
% end of the list and points at the coordinates around the placed stars.
% Receives: Board, List of Coordinates of length 3
% [(R1, C1), (R2, C2), (R3, C3)]
% Returns: True if Board has stars at coordinates
% (R1, C1) and (R3, C3) and points at the adjacent coordinates.
applyPatternI(Board, [(R1, C1), (_, _), (R3, C3)]) :-
  insertMultipleObjects([(R1, C1), (R3, C3)], Board, [s, s]),
  maplist(insertPointsAround(Board), [(R1, C1), (R3, C3)]).

% applyPatternT/2
% Applies a T-shaped pattern to the given Board. 
% The pattern is defined by a list of four coordinates.
% The pattern can be in one of the following orientations:
%   1. Vertical T with the base at the bottom.
%   2. Horizontal T with the base on the right.
%   3. Vertical T with the base at the top.
% Recieves: Board
% Returns: A list of four coordinates defining the T-shaped pattern.
applyPatternT(Board, [(R1, C1), (R2, C2), (R3, C3), (R4, C4)]) :-
  R2 is R1 + 1, R3 is R2, R4 is R3 + 1, 
  (C2 is C1, C3 is C1 + 1, C4 is C1; C2 is C1 - 1, C3 is C1, C4 is C1), !,
  insertMultipleObjects([(R1, C1), (R4, C4)], Board, [s, s]),
  insertPointsAround(Board, (R1, C1)),
  insertPointsAround(Board, (R4, C4)).
applyPatternT(Board, [(R1, C1), (R2, C2), (R3, C3), (R4, C4)]) :-
  R2 is R1, R3 is R2, R4 is R1 + 1, 
  C2 is C1 + 1, C3 is C2 + 1, C4 is C2, !,
  insertMultipleObjects([(R1, C1), (R3, C3)], Board, [s, s]),
  insertPointsAround(Board, (R1, C1)),
  insertPointsAround(Board, (R3, C3)).
applyPatternT(Board, [(R1, C1), (R2, C2), (R3, C3), (R4, C4)]) :-
  R2 is R1 + 1, R3 is R2, R4 is R1, 
  C2 is C1 - 1, C3 is C2 + 1, C4 is C3 + 1, !,
  insertMultipleObjects([(R2, C2), (R4, C4)], Board, [s, s]),
  insertPointsAround(Board, (R2, C2)),
  insertPointsAround(Board, (R4, C4)).

% applyPatterns/2
% Applies the I and T patterns on the board.
% Receives: Board, List of Lists of Coordinates (CoordLists)
% Returns: True if Board is a board and CoordLists is a list of lists of
% coordinates, after applying this predicate, sequences of length 3 will have
% applied applyPatternI/2, or sequences of length 4
% will have applied applyPatternT/2.
applyPatterns(_, []).
applyPatterns(Board, [CoordList|Rest]) :-
  findSequence(Board, 3, CoordList, Seq),
  applyPatternI(Board, Seq), !,
  applyPatterns(Board, Rest).
applyPatterns(Board, [CoordList|Rest]) :-
  findSequence(Board, 4, CoordList, Seq),
  applyPatternT(Board, Seq), !,
  applyPatterns(Board, Rest).
applyPatterns(Board, [_|Rest]) :- 
  !, applyPatterns(Board, Rest).


/* Final Resolution */

% solve/2
% Solves the StarBattle puzzle for the case of two stars.
% Receives: Structures (Board Regions), Board
% Returns: Solved board or the most solved possible using the
% previously implemented strategies.
solve(Structures, Board) :-
  coordinatesVars(Board, VarsList),
  length(VarsList, NumVars),
  % If there are no free coordinates, the board is complete
  % and the predicate terminates.
  (NumVars == 0 -> true;
    allCoords(Structures, CT),
    applyPatterns(Board, CT),
    closeLists(Board, CT),
    % If the number of variables does not change after applying applyPatterns
    % and close, then the board no longer changes and the predicate terminates.
    coordinatesVars(Board, NewVarsList),
    length(NewVarsList, NewNumVars),
    (NumVars == NewNumVars -> true;
    solve(Structures, Board))), !.
