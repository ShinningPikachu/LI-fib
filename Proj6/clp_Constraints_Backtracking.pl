:- use_module(library(clpfd)).

% En swi Prolog la declaracion de constraints con el # se deshace bajo backtracking.

% Por ejemplo, para expresar que la lista de enteros L esta ordenada, hay que hacerlo
% recursivamente, como en sortedOK, y NO bajo backtracking, como en sortedMal:

sortedOK([_]).                                          %%%% OK!
sortedOK([X,Y|L]):- X #< Y, sortedOK([Y|L]).

sortedMal(L):- append(_, [X,Y|_], L), X #< Y, fail.     %%%% <------   asi no
sortedMal(_).                                           %%%% <------ funcionaaa!!!

main:-
    length(L,5),
    L ins 1..6,
    sortedOK(L),
%    sortedMal(L),
    label(L), write(L), nl, halt.

