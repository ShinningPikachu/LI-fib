
%% Disponemos de un grifo de agua, un cubo de 5 litros y otro de 8
%% litros. Se puede verter el contenido de un cubo en otro, llenar un
%% cubo, o vaciar un cubo del todo y queremos saber la secuencia
%% mínima de operaciones para obtener exactamente 4 litros de agua en
%% el cubo de 8 litros.

unPaso(1, [N,M], [5,M]   ):- N < 5. % omplir la galleda de 5 litres
unPaso(1, [N,M], [N,8]   ):- M < 8. % omplir la galleda de 8 litres
unPaso(1, [N,M], [0,M]   ):- N > 0. % buidar la galleda de 5 litres
unPaso(1, [N,M], [N,0]   ):- M > 0. % buidar la galleda de 8 litres
unPaso(1, [N,M], [0,M1]  ):- N > 0, M1 is N+M, M1 =< 8. % portar tota l'aigua dla galleda de 5 litres al de 8
unPaso(1, [N,M], [N1,0]  ):- M > 0, N1 is N+M, N1 =< 5. % portar tota l'aigua dla galleda de 8 litres al de 5
unPaso(1, [N,M], [N1,M1] ):- M < 8, S is N+M, S > 8, D is 8-M, N1 is N-D, M1 is M+D. % portar l'aigua qui cap la galleda de 5 litres al de 8
unPaso(1, [N,M], [N1,M1] ):- N < 5, S is N+M, S > 5, D is 5-N, N1 is N+D, M1 is M-D. % portar l'aigua qui cap la galleda de 8 litres al de 5

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main :- EstadoInicial = [0, 0], EstadoFinal = [0, 4],
        between(1, 1000, CosteMax), % Buscamos solución de coste 0; si no, de 1, etc.
        camino( CosteMax, EstadoInicial, EstadoFinal, [EstadoInicial], Camino ),
        reverse(Camino, Camino1), write(Camino1), write(' con coste '), write(CosteMax), nl, halt.

camino( 0, E,E, C,C ). % Caso base: cuando el estado actual es el estado final.
camino( CosteMax, EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal ) :-
    CosteMax > 0,
    unPaso( CostePaso, EstadoActual, EstadoSiguiente ),
    \+ member( EstadoSiguiente, CaminoHastaAhora ),
    CosteMax1 is CosteMax-CostePaso,
    camino(CosteMax1, EstadoSiguiente, EstadoFinal, [EstadoSiguiente | CaminoHastaAhora], CaminoTotal).
