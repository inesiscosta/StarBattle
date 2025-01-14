:- use_module(library(plunit)).

% Test suite for various predicates
:- begin_tests(predicates).

% 1 - visualize/1

test(visualize_1) :- 
  with_output_to(string(S),
  visualize([1, a, v])),
  assertion(S == "1\na\nv\n").

test(visualize_2) :- 
  with_output_to(string(S),
  visualize([[1, 7], [(a, 9), (4, 6)], v])),
  assertion(S == "[1,7]\n[(a,9),(4,6)]\nv\n").

% 2 - visualizeLine/1

test(visualizeLine_1) :- 
  with_output_to(string(S),
  visualizeLine([1, a, v])),
  assertion(S == "1: 1\n2: a\n3: v\n").

test(visualizeLine_2) :- 
  with_output_to(string(S),
  visualizeLine([[1, 7], [(a, 9), (4, 6)], v])),
  assertion(S == "1: [1,7]\n2: [(a,9),(4,6)]\n3: v\n").

% 3 - insertObject/3

test(insertObject_1) :- 
  Tab = [[_, s, _], [_, p, p], [_, _, p]],
  insertObject((1,3), Tab, ola),
  clearBoard(Tab, T),
  assertion(T == [[v,s,ola],[v,p,p],[v,v,p]]).

test(insertObject_2) :- 
  Tab = [[_, s, _], [_, p, p], [_, _, p]],
  insertObject((1,2), Tab, ola),
  clearBoard(Tab, T),
  assertion(T == [[v,s,v],[v,p,p],[v,v,p]]).

test(insertObject_3) :- 
  Tab = [[_, s, _], [_, p, p], [_, _, p]],
  insertObject((1,4), Tab, ola),
  clearBoard(Tab, T),
  assertion(T == [[v,s,v],[v,p,p],[v,v,p]]).

test(insertObject_4) :- 
  Tab = [[_, s, _], [_, p, p], [_, _, p]],
  insertObject((0,4), Tab, ola),
  clearBoard(Tab, T),
  assertion(T == [[v,s,v],[v,p,p],[v,v,p]]).

% 4 - insertMultipleObjects/3

test(insertMultipleObjects_1) :- 
  Tab = [[_, s, _], [_, p, p], [_, _, p]],
  insertMultipleObjects([(1,3), (4,5), (1, 2), (1, 1)], Tab, [ola, ole, oi, batata]),
  clearBoard(Tab, T),
  assertion(T == [[batata,s,ola],[v,p,p],[v,v,p]]).

test(insertMultipleObjects_2, [fail]) :- 
  Tab = [[_, s, _], [_, p, p], [_, _, p]],
  insertMultipleObjects([(1,3), (4,5), (1, 2)], Tab, [ola, ole]).

% 5 - insertPointsAround/2

test(insertPointsAround_1) :- 
  Tab = [[_, s, _], [_, p, p], [_, _, p]],
  insertPointsAround(Tab, (2, 1)),
  clearBoard(Tab, T),
  assertion(T == [[p,s,v],[v,p,p],[p,p,p]]).

% 6 - insertPoints/2

test(insertPoints_1) :- 
  Tab = [[_, s, _], [_, p, p], [_, _, p]],
  insertPoints(Tab, [(1,1), (2,1)]),
  clearBoard(Tab, T),
  assertion(T == [[p,s,v],[p,p,p],[v,v,p]]).

% 7 - objectsInCoordinates/3

test(objectsInCoordinates_1) :- 
  Tab = [[_, s, _], [_, p, p], [_, _, p]],
  objectsInCoordinates([(1, 1), (2, 3), (3, 3)], Tab, Obj),
  assertion(Obj = [_, p, p]).

test(objectsInCoordinates_2, [fail]) :- 
  Tab = [[_, s, _], [_, p, p], [_, _, p]],
  objectsInCoordinates([(1, 1), (2, 4), (3, 3)], Tab, Obj),
  write(Obj).

test(objectsInCoordinates_3) :- 
  sol(9-1, Tab),
  objectsInCoordinates([(1, 3), (3, 4), (5, 6)], Tab, Obj),
  assertion(Obj == [s,p,p]).

% 8 - coordObjects/5

test(coordObjects_1) :- 
  sol(9-1, Tab),
  coordObjects(s, Tab, [(1, 3), (3, 4), (5, 6)], LCO, Num),
  assertion(Tab == [
    [p,p,s,p,s,p,p,p,p],
    [s,p,p,p,p,p,s,p,p],
    [p,p,p,p,s,p,p,p,s],
    [p,s,p,p,p,p,s,p,p],
    [p,p,p,s,p,p,p,p,s],
    [p,s,p,p,p,s,p,p,p],
    [p,p,p,s,p,p,p,s,p],
    [s,p,p,p,p,s,p,p,p],
    [p,p,s,p,p,p,p,s,p]]),
  assertion(LCO == [(1,3)]),
  assertion(Num == 1).

test(coordObjects_2) :- 
  sol(9-1, Tab),
  coordObjects(p, Tab, [(1, 3), (3, 4), (5, 6)], LCO, Num),
  assertion(Tab == [
    [p,p,s,p,s,p,p,p,p],
    [s,p,p,p,p,p,s,p,p],
    [p,p,p,p,s,p,p,p,s],
    [p,s,p,p,p,p,s,p,p],
    [p,p,p,s,p,p,p,p,s],
    [p,s,p,p,p,s,p,p,p],
    [p,p,p,s,p,p,p,s,p],
    [s,p,p,p,p,s,p,p,p],
    [p,p,s,p,p,p,p,s,p]]),
  assertion(LCO == [(3,4),(5,6)]),
  assertion(Num == 2).

% 9 - coordinatesVars/2

test(coordinatesVars_1) :- 
  Tab = [[_, s, s], [_, p, p], [_, _, p]],
  coordinatesVars(Tab, VarsList),
  assertion(VarsList == [(1,1),(2,1),(3,1),(3,2)]).

% 10 - closeCoordList/2

test(closeCoordList_1) :- 
  Tab = [[_, s, s], [_, p, p], [_, _, p]],
  closeCoordList(Tab, [(1, 1), (1, 2), (1, 3)]),
  clearBoard(Tab, T),
  write(T),
  assertion(T == [[p,s,s],[v,p,p],[v,v,p]]).

test(closeCoordList_2) :- 
  Tab = [[_, s, s], [_, p, p], [_, _, p]],
  closeCoordList(Tab, [(1, 2), (2, 2), (3, 2)]),
  clearBoard(Tab, T),
  write(T),
  assertion(T == [[v,s,s],[p,p,p],[p,s,p]]).

% 11 - closeLists/2

test(close_1) :- 
  Tab = [[_, s, s], [_, p, p], [_, _, p]],
  closeLists(Tab, [[(1, 2), (2, 2), (3, 2)], [(1, 1), (1, 2), (1, 3)]]),
  assertion(Tab == [[p,s,s],[p,p,p],[p,s,p]]).

% 12 - findSequence/4

test(findSequence_1, [fail]) :- 
  Tab = [[_, s, s], [_, p, p], [_, _, p], [p, _, _]],
  findSequence(Tab, 3, [(1,1), (1,2), (1, 3)], Seq),
  write(Seq).

test(findSequence_2) :- 
  Tab = [[_, s, s], [_, p, p], [_, _, p]],
  findSequence(Tab, 3, [(1,1), (2,1), (3, 1)], Seq),
  assertion(Seq == [(1,1),(2,1),(3,1)]).

test(findSequence_3) :- 
  Tab = [[_, s, s], [_, p, p], [_, _, p]],
  findSequence(Tab, 3, [(1,1), (3, 1), (2, 1)], Seq),
  assertion(Seq == [(1,1),(3,1),(2,1)]).

test(findSequence_4) :- 
  Tab = [[_, s, s], [_, p, p], [_, _, p], [p, _, _]],
  findSequence(Tab, 3, [(1,1), (2,1), (3, 1)], Seq),
  assertion(Seq == [(1,1),(2,1),(3,1)]).

test(findSequence_5) :- 
  Tab = [[_, s, s], [_, p, p], [_, _, p]],
  findSequence(Tab, 3, [(1,1), (2,1), (3, 2)], Seq),
  assertion(Seq == [(1,1),(2,1),(3,2)]).

% 13 - applyPatternI/2

test(applyPatternI_1) :- 
  Tab = [[_, s, s], [_, p, p], [_, _, p]],
  applyPatternI(Tab, [(1,1), (2,1), (3, 1)]),
  assertion(Tab == [[s,s,s],[p,p,p],[s,p,p]]).

test(applyPatternI_2, [nondet]) :- 
  Tab = [[_, s, s], [_, p, p], [_, _, p], [_, _, _]],
  applyPatternI(Tab, [(4,1), (4,2), (4, 3)]),
  clearBoard(Tab, T),
  assertion(T == [[v,s,s],[v,p,p],[p,p,p],[s,p,s]]).

% 14 - applyPatterns/2

test(applyPatterns_1, [nondet]) :- 
  regions(9-2, E),
  initial(9, Tab),
  allCoords(E, CT),
  applyPatterns(Tab, CT),
  clearBoard(Tab, T),
  assertion(T == [
    [v,p,p,p,v,p,s,p,s],
    [v,p,s,p,p,p,p,p,p],
    [v,p,p,p,s,p,v,v,v],
    [v,p,s,p,p,p,v,v,v],
    [v,p,p,p,s,p,v,v,v],
    [v,v,v,p,p,p,v,v,v],
    [v,v,v,v,v,v,v,v,v],
    [v,v,p,p,p,p,p,v,v],
    [v,v,p,s,p,s,p,v,v]]).

% 15 - solve/2

test(solve_1, [nondet]) :- 
  regions(9-2, E),
  initial(9, Tab),
  solve(E, Tab),
  clearBoard(Tab, T),
  assertion(T == [
    [p,p,p,p,p,p,s,p,s],
    [s,p,s,p,p,p,p,p,p],
    [p,p,p,p,s,p,v,v,v],
    [s,p,s,p,p,p,p,p,p],
    [p,p,p,p,s,p,v,v,v],
    [p,s,p,p,p,p,p,v,v],
    [p,p,p,s,p,s,p,p,p],
    [p,s,p,p,p,p,p,v,v],
    [p,p,p,s,p,s,p,p,p]]).

test(solve_2, [nondet]) :- 
  regions(9-2, E),
  initial(9-2-1, Tab),
  solve(E, Tab),
  clearBoard(Tab, T),
  assertion(T == [
    [p,p,p,p,p,p,s,p,s],
    [s,p,s,p,p,p,p,p,p],
    [p,p,p,p,s,p,p,s,p],
    [s,p,s,p,p,p,p,p,p],
    [p,p,p,p,s,p,s,p,p],
    [p,s,p,p,p,p,p,p,s],
    [p,p,p,s,p,s,p,p,p],
    [p,s,p,p,p,p,p,s,p],
    [p,p,p,s,p,s,p,p,p]]).

test(solve_3) :- 
  regions(9-24, E),
  initial(9-24-1, Tab),
  solve(E, Tab),
  clearBoard(Tab, T),
  assertion(T == [
    [p,p,p,p,s,p,p,s,p],
    [s,p,s,p,p,p,p,p,p],
    [p,p,p,p,p,s,p,s,p],
    [p,s,p,s,p,p,p,p,p],
    [p,p,p,p,p,p,s,p,s],
    [p,p,s,p,s,p,p,p,p],
    [s,p,p,p,p,p,s,p,p],
    [p,p,p,s,p,p,p,p,s],
    [p,s,p,p,p,s,p,p,p]]).

:- end_tests(predicates).
