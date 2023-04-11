%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 1. A més de l'intèrpret de Prolog, també hi ha un compilador de Prolog que
%% genera un fitxer executable a partir del codi font.

%% Feu servir el Makefile que us passem.

%% swipl -q -O -g main --stand_alone=true -o $(file) -c $(file).pl
%%       ^ activem mode silenciós          ^ especifiquem nom fitxer executable
%%           ^ optimitzem el codi                     ^ especifica nom fitxer font
%% 	     ^ especifiquem per quin predicat comença l'execució
%% 	            ^ especifiquem que volem un executable 'independent'

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 2. Predicat between

%% between(L, U, X) és cert quan X és un enter entre els enters L i U.

%% ?- between(1, 3, 2).
%% true.

%% ?- between(1, 3, X), write(X), nl, fail.
%% 1
%% 2
%% 3
%% false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 3. Predicat findall

%% findall(X, G, L) és cert quan L és la llista del X per als que
%% (hi ha manera d'instanciar les altres variables, si n'hi ha)
%% l'objectiu G és cert

f(a, b, c).
f(a, b, d).
f(b, c, e).
f(b, c, f).
f(c, c, g).

%% ?- findall(C, f(A, B, C), Cs).
%% Cs = [c, d, e, f, g].

%% ?- findall(C, f(a, B, C), Cs).
%% Cs = [c, d].

%% ?- findall(C, f(A, c, C), Cs).
%% Cs = [e, f, g].


%% ?- findall(C, f(A, a, C), Cs).
%% Cs = [].

%% Hi ha altres predicats semblants al findall: setof, bagof. Però tenen
%% el problema que, si no hi ha solucions per a G, aleshores fallen, en
%% lloc de considerar que L ha de ser la llista buida.
%% Això sol donar problemes, i no en recomanem l'ús.


%% ?- findall(A, f(A, b, C), As).
%% As = [a, a].

%% Pot ser que hi hagi solucions repetides!
%% Si no volem repeticions, podem fer servir el predicat sort,
%% que ordena *i elimina repeticions*:

%% ?- | sort([1,3,2], X).
%% X = [1, 2, 3].

%% ?- sort([1,3,1,3,2,2], X).
%% X = [1, 2, 3].

%% ?- findall(A, f(A, b, C), As), sort(As, AsSenseRepeticions).
%% As = [a, a],
%% AsSenseRepeticions = [a].


%% Més exemples, combinant findall i between:

%% ?- findall(X, (between(1,7,X), 1 is X mod 2), L).   %% llista dels senars entre 1 i 7
%% L = [1, 3, 5, 7].

%% ?- findall(Y, (between(1,7,X), 1 is X mod 2, Y is X*X), L).   %% llista dels quadrats dels senars entre 1 i 7
%% L = [1, 9, 25, 49].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 4. Resolució de problemes combinatoris codificant en SAT
%% i usant Prolog com a llenguatge de modelatge

%% Farem servir el problema d'omplir un Sudoku com a exemple: miSudoku.pl

%% Què fa miSudoku.pl?

%% > ./miSudoku
%% Generated 12016 clauses over 729 variables.
%% Calling solver....
%% Solution found:

%% 6 4 3  9 7 5  2 1 8
%% 2 7 8  4 3 1  9 5 6
%% 5 1 9  6 8 2  3 4 7

%% 3 2 5  8 6 4  1 7 9
%% 1 8 7  3 5 9  4 6 2
%% 9 6 4  2 1 7  5 8 3

%% 4 5 2  7 9 8  6 3 1
%% 8 3 1  5 2 6  7 9 4
%% 7 9 6  1 4 3  8 2 5


%% Noteu que el nombre de variables és >> 300:


%%     1. genera i guarda en un fitxer .cnf les clàusules corresponents a
%%     les restriccions del problema:

%%         genera fitxers clause, header, i infile.cnf (infile.cnf = header+clauses)

%%         En el programa es defineixen unes SAT-variables (variables
%%         proposicionals) que permeten representar les solucions del
%%         problema. Per tal de descartar assignacions de valors a les
%%         variables que no sigui solucions (no es compleixin les
%%         restriccions), afegim clàusules.

%%         El programa prolog automàticament transforma aquestes
%%         SAT-variables en variables numèriques que el SAT-solver
%%         picosat pot entendre, de forma que en infile.cnf tenim un
%%         conjunt de clàusules (amb variable numèriques) en el mateix
%%         format que les fórmules en CNF de la pràctica 1.

%% > head infile.cnf
%% p cnf 729 12016
%% 1 0
%% 2 0
%% 3 0
%% ...

%% > tail infile.cnf
%% -641 -650 0
%% -641 -712 0
%% -641 -720 0
%% ...


%%     2. el programa llavors crida al SAT-solver picosat amb infile.cnf,
%%     i guarda el resultat (si és SAT) en un fitxer de nom 'model'

%%     3. obté un model simbòlic M, traduint cap enrere les variables
%%     proposicionals "numèriques" del model de picosat en les variables
%%     simbòliques (SAT-variables).

%%         El model simbòlic M només conté les que són certes i que
%%         permeten definir la solució del problema original.

%%     4. es "visualitza" la solució de forma fàcilment llegible per
%%     nosaltres (displaySol)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% La idea és que reaprofiteu el fitxer miSudoku.pl com a plantilla per a
%% resoldre els problemes d'aquesta sessió (olympiad, roads, tsp).

%% Per codificar un problema només heu de modificar tres punts de l'esquema:

%%    1) definir SAT-variables (moltes vegades ja vénen donades, o donem algunes SAT-variables, i cal definir-ne alguna altra)

%%    2) generar les clàusules corresponents a les restriccions: completar el predicat  writeClauses/0. *No el confoneu amb writeClause/1*, que afegeix una clàusula al fitxer clauses

%%    3) escriure o completar displaySol(M) on M és la llista de variables simbòliques [SAT-variables] que són *certes* en el model retornat per picosat (sovint també ve donat)

%% Alguns predicats interessants:
%%       main, treatResult
%%       predicats que generen els cardinality constraints: atLeast, atMost, exactly.
%%       Quin és el número de clàusules que es generen amb aquesta codificació?

%% Preguntes:

%% * perquè serveix el predicat symbolicOutput?

%% * perquè/quan surt el missatge 'writeClauses failed!'?

%% * perquè/quan surt el missatge 'cnf input error. Wrote anything strange in your cnf?'
