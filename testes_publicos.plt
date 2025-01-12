:- use_module(library(plunit)).

% Test suite for various predicates
:- begin_tests(predicates).

% 1-vizualiza

test(vizualiza_1) :- with_output_to(string(S), visualiza([1, a, v])),
                     assertion(S == "1\na\nv\n").

test(vizualiza_2) :- with_output_to(string(S), visualiza([[1, 7], [(a, 9), (4, 6)], v])),
                     assertion(S == "[1,7]\n[(a,9),(4,6)]\nv\n").

% 2-vizualiza linha

test(visualizaLinha_1) :- with_output_to(string(S), visualizaLinha([1, a, v])),
                          assertion(S == "1: 1\n2: a\n3: v\n").

test(visualizaLinha_2) :- with_output_to(string(S), visualizaLinha([[1, 7], [(a, 9), (4, 6)], v])),
                           assertion(S == "1: [1,7]\n2: [(a,9),(4,6)]\n3: v\n").

% 3-insereObjecto

test(insereObjecto_1) :- Tab = [[_, e, _], [_, p, p], [_, _, p]],
                         insereObjecto((1,3), Tab, ola),
                         limpaTabuleiro(Tab, T),
                         assertion(T == [[v,e,ola],[v,p,p],[v,v,p]]).

test(insereObjecto_2) :- Tab = [[_, e, _], [_, p, p], [_, _, p]],
                         insereObjecto((1,2), Tab, ola),
                         limpaTabuleiro(Tab, T),
                         assertion(T == [[v,e,v],[v,p,p],[v,v,p]]).

test(insereObjecto_3) :- Tab = [[_, e, _], [_, p, p], [_, _, p]],
                         insereObjecto((1,4), Tab, ola),
                         limpaTabuleiro(Tab, T),
                         assertion(T == [[v,e,v],[v,p,p],[v,v,p]]).

test(insereObjecto_4) :- Tab = [[_, e, _], [_, p, p], [_, _, p]],
                         insereObjecto((0,4), Tab, ola),
                         limpaTabuleiro(Tab, T),
                         assertion(T == [[v,e,v],[v,p,p],[v,v,p]]).

% 4-insereVariosObjectos

test(insereVariosObjectos_1) :- Tab = [[_, e, _], [_, p, p], [_, _, p]],
                                insereVariosObjectos([(1,3), (4,5), (1, 2), (1, 1)], Tab, [ola, ole, oi, batata]),
                                limpaTabuleiro(Tab, T),
                                assertion(T == [[batata,e,ola],[v,p,p],[v,v,p]]).

test(insereVariosObjectos_2, [fail]) :- Tab = [[_, e, _], [_, p, p], [_, _, p]],
                                        insereVariosObjectos([(1,3), (4,5), (1, 2)], Tab, [ola, ole]).

% 5-inserePontosVolta

test(inserePontosVolta_1) :- Tab = [[_, e, _], [_, p, p], [_, _, p]],
                           inserePontosVolta(Tab, (2, 1)),
                           limpaTabuleiro(Tab, T),
                           assertion(T == [[p,e,v],[v,p,p],[p,p,p]]).

% 6-inserePontos

test(inserePontos_1) :- Tab = [[_, e, _], [_, p, p], [_, _, p]],
                        inserePontos(Tab, [(1,1), (2,1)]),
                        limpaTabuleiro(Tab, T),
                        assertion(T == [[p,e,v],[p,p,p],[v,v,p]]).

% 7-objectosEmCoordenadas

test(objectosEmCoordenadas_1) :- Tab = [[_, e, _], [_, p, p], [_, _, p]],
                                 objectosEmCoordenadas([(1, 1), (2, 3), (3, 3)], Tab, Obj),
                                 assertion(Obj = [_, p, p]).

test(objectosEmCoordenadas_2, [fail]) :- Tab = [[_, e, _], [_, p, p], [_, _, p]],
                                         objectosEmCoordenadas([(1, 1), (2, 4), (3, 3)], Tab, Obj),
                                         write(Obj).

test(objectosEmCoordenadas_3) :- sol(9-1, Tab),
                                 objectosEmCoordenadas([(1, 3), (3, 4), (5, 6)], Tab, Obj),
                                 assertion(Obj == [e,p,p]).

% 8-coordObjectos

test(coordObjectos_1) :- sol(9-1, Tab),
                         coordObjectos(e, Tab, [(1, 3), (3, 4), (5, 6)], LCO, Num),
                         assertion(Tab == [[p,p,e,p,e,p,p,p,p],[e,p,p,p,p,p,e,p,p],[p,p,p,p,e,p,p,p,e],[p,e,p,p,p,p,e,p,p],[p,p,p,e,p,p,p,p,e],[p,e,p,p,p,e,p,p,p],[p,p,p,e,p,p,p,e,p],[e,p,p,p,p,e,p,p,p],[p,p,e,p,p,p,p,e,p]]),
                         assertion(LCO == [(1,3)]),
                         assertion(Num == 1).

test(coordObjectos_2) :- sol(9-1, Tab),
                         coordObjectos(p, Tab, [(1, 3), (3, 4), (5, 6)], LCO, Num),
                         assertion(Tab == [[p,p,e,p,e,p,p,p,p],[e,p,p,p,p,p,e,p,p],[p,p,p,p,e,p,p,p,e],[p,e,p,p,p,p,e,p,p],[p,p,p,e,p,p,p,p,e],[p,e,p,p,p,e,p,p,p],[p,p,p,e,p,p,p,e,p],[e,p,p,p,p,e,p,p,p],[p,p,e,p,p,p,p,e,p]]),
                         assertion(LCO == [(3,4),(5,6)]),
                         assertion(Num == 2).

% 9-coordenadasVars

test(coordenadasVars_1) :- Tab = [[_, e, e], [_, p, p], [_, _, p]],
                           coordenadasVars(Tab, ListaVars),
                           assertion(ListaVars == [(1,1),(2,1),(3,1),(3,2)]).

% 10-fechaListaCoordenadas

test(fechaListaCoordenadas_1) :- Tab = [[_, e, e], [_, p, p], [_, _, p]],
                                 fechaListaCoordenadas(Tab, [(1, 1), (1, 2), (1, 3)]),
                                 limpaTabuleiro(Tab, T),
                                 write(T),
                                 assertion(T == [[p,e,e],[v,p,p],[v,v,p]]).

test(fechaListaCoordenadas_2) :- Tab = [[_, e, e], [_, p, p], [_, _, p]],
                                 fechaListaCoordenadas(Tab, [(1, 2), (2, 2), (3, 2)]),
                                 limpaTabuleiro(Tab, T),
                                 write(T),
                                 assertion(T == [[v,e,e],[p,p,p],[p,e,p]]).

% 11-fecha

test(fecha_1) :- Tab = [[_, e, e], [_, p, p], [_, _, p]],
                 fecha(Tab, [[(1, 2), (2, 2), (3, 2)], [(1, 1), (1, 2), (1, 3)]]),
                 assertion(Tab == [[p,e,e],[p,p,p],[p,e,p]]).

% 12-encontraSequencia

test(encontraSequencia_1, [fail]) :- Tab = [[_, e, e], [_, p, p], [_, _, p], [p, _, _]],
                                     encontraSequencia(Tab, 3, [(1,1), (1,2), (1, 3)], Seq),
                                     write(Seq).

test(encontraSequencia_2) :- Tab = [[_, e, e], [_, p, p], [_, _, p]],
                             encontraSequencia(Tab, 3, [(1,1), (2,1), (3, 1)], Seq),
                             assertion(Seq == [(1,1),(2,1),(3,1)]).

test(encontraSequencia_3) :- Tab = [[_, e, e], [_, p, p], [_, _, p]],
                             encontraSequencia(Tab, 3, [(1,1), (3, 1), (2, 1)], Seq),
                             assertion(Seq == [(1,1),(3,1),(2,1)]).

test(encontraSequencia_4) :- Tab = [[_, e, e], [_, p, p], [_, _, p], [p, _, _]],
                             encontraSequencia(Tab, 3, [(1,1), (2,1), (3, 1)], Seq),
                             assertion(Seq == [(1,1),(2,1),(3,1)]).

test(encontraSequencia_5) :- Tab = [[_, e, e], [_, p, p], [_, _, p]],
                             encontraSequencia(Tab, 3, [(1,1), (2,1), (3, 2)], Seq),
                             assertion(Seq == [(1,1),(2,1),(3,2)]).

% 13-aplicaPadraoI

test(aplicaPadraoI_1) :- Tab = [[_, e, e], [_, p, p], [_, _, p]],
                         aplicaPadraoI(Tab, [(1,1), (2,1), (3, 1)]),
                         assertion(Tab == [[e,e,e],[p,p,p],[e,p,p]]).

test(aplicaPadraoI_2, [nondet]) :- Tab = [[_, e, e], [_, p, p], [_, _, p], [_, _, _]],
                                   aplicaPadraoI(Tab, [(4,1), (4,2), (4, 3)]),
                                   limpaTabuleiro(Tab, T),
                                   assertion(T == [[v,e,e],[v,p,p],[p,p,p],[e,p,e]]).

% 14-aplicaPadroes

test(aplicaPadroes_1, [nondet]) :- regioes(9-2, E),
                                   inicial(9, Tab),
                                   coordTodas(E, CT),
                                   aplicaPadroes(Tab, CT),
                                   limpaTabuleiro(Tab, T),
                                   assertion(T == [[v,p,p,p,v,p,e,p,e],[v,p,e,p,p,p,p,p,p],[v,p,p,p,e,p,v,v,v],[v,p,e,p,p,p,v,v,v],[v,p,p,p,e,p,v,v,v],[v,v,v,p,p,p,v,v,v],[v,v,v,v,v,v,v,v,v],[v,v,p,p,p,p,p,v,v],[v,v,p,e,p,e,p,v,v]]).

% 15-resolve

test(resolve_1, [nondet]) :- regioes(9-2, E),
                             inicial(9, Tab),
                             resolve(E, Tab),
                             limpaTabuleiro(Tab, T),
                             assertion(T == [[p,p,p,p,p,p,e,p,e],[e,p,e,p,p,p,p,p,p],[p,p,p,p,e,p,v,v,v],[e,p,e,p,p,p,p,p,p],[p,p,p,p,e,p,v,v,v],[p,e,p,p,p,p,p,v,v],[p,p,p,e,p,e,p,p,p],[p,e,p,p,p,p,p,v,v],[p,p,p,e,p,e,p,p,p]]).

test(resolve_2, [nondet]) :- regioes(9-2, E),
                             inicial(9-2-1, Tab),
                             resolve(E, Tab),
                             limpaTabuleiro(Tab, T),
                             assertion(T == [[p,p,p,p,p,p,e,p,e],[e,p,e,p,p,p,p,p,p],[p,p,p,p,e,p,p,e,p],[e,p,e,p,p,p,p,p,p],[p,p,p,p,e,p,e,p,p],[p,e,p,p,p,p,p,p,e],[p,p,p,e,p,e,p,p,p],[p,e,p,p,p,p,p,e,p],[p,p,p,e,p,e,p,p,p]]).

test(resolve_3) :- regioes(9-24, E),
                   inicial(9-24-1, Tab),
                   resolve(E, Tab),
                   limpaTabuleiro(Tab, T),
                   assertion(T == [[p,p,p,p,e,p,p,e,p],[e,p,e,p,p,p,p,p,p],[p,p,p,p,p,e,p,e,p],[p,e,p,e,p,p,p,p,p],[p,p,p,p,p,p,e,p,e],[p,p,e,p,e,p,p,p,p],[e,p,p,p,p,p,e,p,p],[p,p,p,e,p,p,p,p,e],[p,e,p,p,p,e,p,p,p]]).

:- end_tests(predicates).

