prod([],1).
prod(L,P):- L = [X|L1], prod(L1, P1), P is P1*X.

pescalar([],[],0).
pescalar(L1, L2, P):- L1 = [X|XS], L2 = [Y|YS], pescalar(XS,YS,P1), P is P1 + X * Y. 

intersec([],_,[]).
intersec(L1, L2, L3):- L1 = [X|XS], member(X, L2), intersec(XS, L2, LX), L3 = [X|LX].
intersec(L1, L2, L3):- L1 = [X|XS], not(member(X, L2)), intersec(XS, L2, LX), L3 = LX.

union([],L,L).
union(L1, L2, L3):- L1 = [X|XS], member(X, L2), union(XS, L2, LX), L3 = LX.
union(L1, L2, L3):- L1 = [X|XS], not(member(X, L2)), union(XS, L2, LX), L3 = [X|LX].

%first(L, X):- L = (Y|[_]), X is Y.

end(L, X) :- append(_, [X], L).

inverse([],[]).
inverse(L, [X|RL]) :- end(L, X), removeLast(L, NL), inverse(NL, RL).

removeLast(L, NL) :- append(NL, [_], L).

fib(1, 1):- !.
fib(2, 1):- !.
fib(N, F):- N1 is N-1, N2 is N - 2, fib(N1, AF), fib(N2, BF), F is AF + BF.

dados(0,0,[]).
dados(P, N, [X|L]):- N > 0, member(X, [1,2,3,4,5,6]), P1 is P - X, N1 is N - 1, dados(P1, N1, L).

suma([], 0).
suma([L|LX], X) :- suma(LX, X1), X is X1 + L.

suma_demas([]) :- false.
suma_demas(L) :- suma(L, X), member(Y, L), H is X - Y, H = Y, !.

%concat([],L,L).
%concat([X|L1],L2,[X|L3]):- concat(L1,L2,L3).

suma_ants([]) :- false.
suma_ants(L) :- append(L1,[X|_],L), suma(L1,Y), X = Y, !.

elimina([], _, []).
elimina([G|L], X, R) :- G = X, elimina(L, X, R).
elimina([G|L], X, [G|R]) :- G \= X, elimina(L, X, R). 

size([], 0).
size([_|L], S) :- size(L, S1), S is S1 + 1.

car([], []).
car([X|L], N) :- elimina(L, X, L1), car(L1, N1), size(L, X1), size(L1, X2), Y is X1 - X2 + 1, N = [[X,Y]|N1].

card([]).
card(L) :- car(L, N), write(N).

esta_ordenada([]).
esta_ordenada([_]):- !.
esta_ordenada([X,X1|L]):- X =< X1, esta_ordenada([X1|L]).

orde(L1,L2):- permutation(L1,L2), esta_ordenada(L2).

anadir(_, [], []).
anadir(X, [L|N1], [P1|P]):- anadir(X, N1, P), concat(X, L, P1).

tot([], _, []).
tot([X|L], N1, N) :- tot(L, N1, NL), anadir(X, N1, NX), append(NX, NL, N).

combina(L, 1, L).
combina(L, X, N) :- Y is X - 1, combina(L, Y, N1), tot(L, N1, N).

diccionario(L, X) :- combina(L, X, N), write(N).

palindromos([]).
palindromos(L) :- permutation(L, L1), reverse(L1, L2), L2 = L1, write(L1).

test(L) :- reverse(L, L1), write(L1).

sendMoreMoney:-  Letters = [S, E, N, D, M, O, R, Y, _, _],
                                Numbers = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
                                permutation(Letters, Numbers),
                                S1 is 1000 * S + 100 * E + 10 * N + D +
                                         1000 * M + 100 * O + 10 * R + E,
                                S1 is 10000 * M + 1000 * O + 100 * N + 10 * E + Y,
                                writeLetters(Letters), ! .
                                
writeLetters([S, E, N, D, M, O, R, Y, _, _]):-
                    write('S = '), write(S), nl,
                    write('E = '), write(E), nl,
                    write('N = '), write(N), nl,
                    write('D = '), write(D), nl,
                    write('M = '), write(M), nl,
                    write('O = '), write(O), nl,                                                 
                    write('R = '), write(R), nl,
                    write('Y = '), write(Y), nl,
                    writeSuma([S, E, N, D, M, O, R, Y]).
                    
                    
writeSuma([S, E, N, D, M, O, R, Y]):-
                    S1 is 1000 * S + 100 * E + 10 * N + D,
                    S2 is 1000 * M + 100 * O + 10 * R + E,
                    S3 is S1 + S2,
                    S4 is  10000 * M + 1000 * O + 100 * N + 10 * E + Y,
                    write('SEND = '), write(S1), 
                    write(' + '), 
                    write('MORE = '), write(S2), nl,
                    write('      = '), write(S3), nl,
                    write( '----------------------------------------' ), nl, 
                    write('MONEY = '), write(S4).
                    
p([],[]).
p(L,[X|P]) :- select(X,L,R), p(R,P).

firstElement([X|_], X).

ok([_|[]]).
ok([(_, X2)|P]) :- firstElement(P, (Y1, _)), X2 = Y1, ok(P). 

dom(L) :- p(L, P), ok(P), write(P), nl.
%dom(_) :- write(’no hay cadena’), nl.














