unPaso(1,[Fil, Col, N], [Fils, Cols, N]) :- Fils is Fil + 1, Cols is Col + 2, Cols >= 0, Cols < N, Fils >= 0, Fils < N.
unPaso(1,[Fil, Col, N], [Fils, Cols, N]) :- Fils is Fil + 1, Cols is Col - 2, Cols >= 0, Cols < N, Fils >= 0, Fils < N.
unPaso(1,[Fil, Col, N], [Fils, Cols, N]) :- Fils is Fil + 2, Cols is Col + 1, Cols >= 0, Cols < N, Fils >= 0, Fils < N.
unPaso(1,[Fil, Col, N], [Fils, Cols, N]) :- Fils is Fil + 2, Cols is Col - 1, Cols >= 0, Cols < N, Fils >= 0, Fils < N.
unPaso(1,[Fil, Col, N], [Fils, Cols, N]) :- Fils is Fil - 1, Cols is Col + 2, Cols >= 0, Cols < N, Fils >= 0, Fils < N.
unPaso(1,[Fil, Col, N], [Fils, Cols, N]) :- Fils is Fil - 1, Cols is Col - 2, Cols >= 0, Cols < N, Fils >= 0, Fils < N.
unPaso(1,[Fil, Col, N], [Fils, Cols, N]) :- Fils is Fil - 2, Cols is Col + 1, Cols >= 0, Cols < N, Fils >= 0, Fils < N.
unPaso(1,[Fil, Col, N], [Fils, Cols, N]) :- Fils is Fil - 2, Cols is Col - 1, Cols >= 0, Cols < N, Fils >= 0, Fils < N.

main :-
	write("Write size of chess table N x N "),
	read(N),
	write("Write initial position X "),
	read(Filini),
	write("Write initial position Y "),
	read(Colini),
	write("Write initial position X "),
	read(Filfi),
	write("Write initial position X "),
	read(Colfi),
	EstadoInicial = [Filini, Colini, N],
	EstadoFinal = [Filfi, Colfi, N],
	write("Write exactly cost "),
	read(CosteMax),
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
