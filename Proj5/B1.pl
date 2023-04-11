unPaso(1, [N, M, N1, M1, 1], [N, B, N1, B1, 0]) :- MP is M - 1, (N == 0 ; N >= MP), MP >= 0, B is M - 1, B1 is M1 + 1.
unPaso(1, [N, M, N1, M1, 1], [N, B, N1, B1, 0]) :- MP is M - 2, (N == 0 ; N >= MP), MP >= 0, B is M - 2, B1 is M1 + 2.
unPaso(1, [N, M, N1, M1, 1], [A, M, A1, M1, 0]) :- NP is N - 1, (NP == 0 ; NP >= M), NP >= 0, A is N - 1, A1 is N1 + 1.
unPaso(1, [N, M, N1, M1, 1], [A, M, A1, M1, 0]) :- NP is N - 2, (NP == 0 ; NP >= M), NP >= 0, A is N - 2, A1 is N1 + 2.
unPaso(1, [N, M, N1, M1, 1], [A, B, A1, B1, 0]) :- NP is N - 1, NP >= 0, MP is M - 1, MP >= 0, (N == 0 ; NP >= MP), A is N - 1, A1 is N1 + 1, B is M - 1, B1 is M1 + 1.

unPaso(1, [N1, M1, N, M, 0], [N1, B1, N, B, 1]) :- MP is M - 1, (N == 0 ; N >= MP), MP >= 0, B is M - 1, B1 is M1 + 1.
unPaso(1, [N1, M1, N, M, 0], [N1, B1, N, B, 1]) :- MP is M - 2, (N == 0 ; N >= MP), MP >= 0, B is M - 2, B1 is M1 + 2.
unPaso(1, [N1, M1, N, M, 0], [A1, M1, A, M, 1]) :- NP is N - 1, (NP == 0 ; NP >= M), NP >= 0, A is N - 1, A1 is N1 + 1.
unPaso(1, [N1, M1, N, M, 0], [A1, M1, A, M, 1]) :- NP is N - 2, (NP == 0 ; NP >= M), NP >= 0, A is N - 2, A1 is N1 + 2.
unPaso(1, [N1, M1, N, M, 0], [A1, B1, A, B, 1]) :- NP is N - 1, NP >= 0, MP is M - 1, MP >= 0, (N == 0 ; NP >= MP), A is N - 1, A1 is N1 + 1, B is M - 1, B1 is M1 + 1.


main :- EstadoInicial = [3, 3, 0, 0, 1],
	EstadoFinal = [0, 0, 3, 3, 0],
	between(1, 1000, CosteMax),
	% Buscamos solución de coste 0; si no, de 1, etc.
	camino( CosteMax, EstadoInicial, EstadoFinal, [EstadoInicial], Camino ),
	reverse(Camino, Camino1), write(Camino1), write(' con coste '), write(CosteMax), nl, halt.

camino( 0, E,E, C,C ).
% Caso base: cuando el estado actual es el estado final.
camino( CosteMax, EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal ) :-
	CosteMax > 0,
	unPaso( CostePaso, EstadoActual, EstadoSiguiente ),
	\+ member( EstadoSiguiente, CaminoHastaAhora ),
	CosteMax1 is CosteMax-CostePaso,
	camino(CosteMax1, EstadoSiguiente, EstadoFinal, [EstadoSiguiente|CaminoHastaAhora], CaminoTotal).


