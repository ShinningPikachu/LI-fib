unPaso(2, [P1, P2, P5, P8, 1], [P1s, P2s, P5, P8, 0]):- P1s is (-P1), P2s is (-P2), P1 > 0, P2 > 0.
unPaso(5, [P1, P2, P5, P8, 1], [P1s, P2, P5s, P8, 0]):- P1s is (-P1), P5s is (-P5), P1 > 0, P5 > 0.
unPaso(8, [P1, P2, P5, P8, 1], [P1s, P2, P5, P8s, 0]):- P1s is (-P1), P8s is (-P8), P1 > 0, P8 > 0.
unPaso(5, [P1, P2, P5, P8, 1], [P1, P2s, P5s, P8, 0]):- P2s is (-P2), P5s is (-P5), P2 > 0, P5 > 0.
unPaso(8, [P1, P2, P5, P8, 1], [P1, P2s, P5, P8s, 0]):- P2s is (-P2), P8s is (-P8), P2 > 0, P8 > 0.
unPaso(8, [P1, P2, P5, P8, 1], [P1, P2, P5s, P8s, 0]):- P5s is (-P5), P8s is (-P8), P5 > 0, P8 > 0.
unPaso(8, [P1, P2, P5, P8, 1], [P1, P2, P5, P8s, 0]):- P8s is (-P8), P8 > 0.
unPaso(5, [P1, P2, P5, P8, 1], [P1, P2, P5s, P8, 0]):- P5s is (-P5), P5 > 0.
unPaso(2, [P1, P2, P5, P8, 1], [P1, P2s, P5, P8, 0]):- P2s is (-P2), P2 > 0.
unPaso(1, [P1, P2, P5, P8, 1], [P1s, P2, P5, P8, 0]):- P1s is (-P1), P1 > 0.

unPaso(2, [P1, P2, P5, P8, 0], [P1s, P2s, P5, P8, 1]):- P1s is (-P1), P2s is (-P2), P1 < 0, P2 < 0.
unPaso(5, [P1, P2, P5, P8, 0], [P1s, P2, P5s, P8, 1]):- P1s is (-P1), P5s is (-P5), P1 < 0, P5 < 0.
unPaso(8, [P1, P2, P5, P8, 0], [P1s, P2, P5, P8s, 1]):- P1s is (-P1), P8s is (-P8), P1 < 0, P8 < 0.
unPaso(5, [P1, P2, P5, P8, 0], [P1, P2s, P5s, P8, 1]):- P2s is (-P2), P5s is (-P5), P2 < 0, P5 < 0.
unPaso(8, [P1, P2, P5, P8, 0], [P1, P2s, P5, P8s, 1]):- P2s is (-P2), P8s is (-P8), P2 < 0, P8 < 0.
unPaso(8, [P1, P2, P5, P8, 0], [P1, P2, P5s, P8s, 1]):- P5s is (-P5), P8s is (-P8), P5 < 0, P8 < 0.
unPaso(8, [P1, P2, P5, P8, 0], [P1, P2, P5, P8s, 1]):- P8s is (-P8), P8 < 0.
unPaso(5, [P1, P2, P5, P8, 0], [P1, P2, P5s, P8, 1]):- P5s is (-P5), P5 < 0.
unPaso(2, [P1, P2, P5, P8, 0], [P1, P2s, P5, P8, 1]):- P2s is (-P2), P2 < 0.
unPaso(1, [P1, P2, P5, P8, 0], [P1s, P2, P5, P8, 1]):- P1s is (-P1), P1 < 0.



main :- EstadoInicial = [1,2,5,8,1],
	EstadoFinal = [-1,-2,-5,-8,0],
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
