% The government of a country with very few but very large roads needs
% to decide where to place gas stations.  It has been decided that if
% a road connects cities C1 and C2, a gas station should be placed in
% C1, C2, or both.  The government budget limits the number of
% stations that can be built (maxStations).
% Complete the following program to do this.

numCities(20).
maxStations(14).
road(18,19).
road(6,18).
road(1,12).
road(11,19).
road(3,15).
road(3,6).
road(2,15).
road(10,15).
road(4,7).
road(7,9).
road(12,13).
road(10,13).
road(13,20).
road(9,12).
road(13,14).
road(11,18).
road(3,8).
road(9,14).
road(19,20).
road(3,5).
road(10,14).
road(17,20).
road(3,11).
road(6,10).
road(9,18).
road(1,10).
road(2,4).
road(4,13).
road(15,17).
road(8,19).
road(2,8).
road(5,14).
road(7,9).
road(8,20).
road(4,10).
road(11,12).
road(7,12).
road(7,18).
road(2,14).
road(10,18).
road(6,11).
road(4,18).
road(6,20).
road(4,11).
road(8,18).
road(9,15).
road(5,13).
road(1,11).
road(10,17).
road(10,16).
road(3,20).
road(1,4).
road(16,18).
road(5,6).
road(11,16).
road(13,18).
road(10,20).
road(2,9).
road(1,18).
road(6,12).
road(9,19).
road(3,9).
road(15,16).
road(11,16).
road(12,16).
road(2,12).
road(12,15).
road(2,11).
road(1,19).
road(15,18).
road(11,13).
road(2,8).
road(2,13).
road(9,14).
road(7,17).
road(16,17).
% end input

%Helpful prolog predicates
city(C):- numCities(N), between(1,N,C).


%%%%%%% =======================================================================================
%
% Our LI Prolog template for solving problems using a SAT solver.
%
% It generates the SAT clauses, calls the SAT solver, and shows the solution. Just specify:
%       1. SAT Variables
%       2. Clause generation
%       3. DisplaySol: show the solution.
%
%%%%%%% =======================================================================================

symbolicOutput(0).

%%%%%%  1. SAT Variables:

satVariable( install(C) ):- city(C). %   install(C) means  "a gas station is installed in city C"


%%%%%%  2. Clause generation for the SAT solver:

writeClauses:- 
	conjuntCiutat,
	minimUnGasolinera,
	true.
writeClauses :- told, nl, write('writeClauses failed!'), nl,nl, halt.


conjuntCiutat :- findall(install(C), city(C), List), maxStations(K), atMost(K, List), fail.
conjuntCiutat.

minimUnGasolinera :- city(A), city(B), road(A,B), writeClause([install(A), install(B)]), fail.
minimUnGasolinera.

%%%%%%  3. DisplaySol: show the solution. Here M contains the literals that are true in the model:

%displaySol(M):- nl, write(M), nl, nl, fail.
displaySol(M):- write("Ciutats amb gasolinera: "), nl, city(C), member(install(C), M), write(C), nl, fail.
displaySol(_):- nl.

%%%%%%% =======================================================================================



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Everything below is given as a standard library, reusable for solving
%    with SAT many different problems.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Express that Var is equivalent to the disjunction of Lits:
expressOr( Var, Lits ):- symbolicOutput(1), write( Var ), write(' <--> or('), write(Lits), write(')'), nl, !.
expressOr( Var, Lits ):- member(Lit,Lits), negate(Lit,NLit), writeClause([ NLit, Var ]), fail.
expressOr( Var, Lits ):- negate(Var,NVar), writeClause([ NVar | Lits ]),!.

%% expressOr(a,[x,y]) genera 3 clausulas (como en la Transformación de Tseitin):
%% a == x v y
%% x -> a       -x v a
%% y -> a       -y v a
%% a -> x v y   -a v x v y

% Express that Var is equivalent to the conjunction of Lits:
expressAnd( Var, Lits) :- symbolicOutput(1), write( Var ), write(' <--> and('), write(Lits), write(')'), nl, !.
expressAnd( Var, Lits):- member(Lit,Lits), negate(Var,NVar), writeClause([ NVar, Lit ]), fail.
expressAnd( Var, Lits):- findall(NLit, (member(Lit,Lits), negate(Lit,NLit)), NLits), writeClause([ Var | NLits]), !.


%%%%%% Cardinality constraints on arbitrary sets of literals Lits:

exactly(K,Lits):- symbolicOutput(1), write( exactly(K,Lits) ), nl, !.
exactly(K,Lits):- atLeast(K,Lits), atMost(K,Lits),!.

atMost(K,Lits):- symbolicOutput(1), write( atMost(K,Lits) ), nl, !.
atMost(K,Lits):-   % l1+...+ln <= k:  in all subsets of size k+1, at least one is false:
      negateAll(Lits,NLits),
      K1 is K+1,    subsetOfSize(K1,NLits,Clause), writeClause(Clause),fail.
atMost(_,_).

atLeast(K,Lits):- symbolicOutput(1), write( atLeast(K,Lits) ), nl, !.
atLeast(K,Lits):-  % l1+...+ln >= k: in all subsets of size n-k+1, at least one is true:
      length(Lits,N),
      K1 is N-K+1,  subsetOfSize(K1, Lits,Clause), writeClause(Clause),fail.
atLeast(_,_).

negateAll( [], [] ).
negateAll( [Lit|Lits], [NLit|NLits] ):- negate(Lit,NLit), negateAll( Lits, NLits ),!.

negate( -Var,  Var):-!.
negate(  Var, -Var):-!.

subsetOfSize(0,_,[]):-!.
subsetOfSize(N,[X|L],[X|S]):- N1 is N-1, length(L,Leng), Leng>=N1, subsetOfSize(N1,L,S).
subsetOfSize(N,[_|L],   S ):-            length(L,Leng), Leng>=N,  subsetOfSize( N,L,S).


%%%%%% main:

main:-  symbolicOutput(1), !, writeClauses, halt.   % print the clauses in symbolic form and halt Prolog
main:-  initClauseGeneration,
        tell(clauses), writeClauses, told,          % generate the (numeric) SAT clauses and call the solver
        tell(header),  writeHeader,  told,
        numVars(N), numClauses(C),
        write('Generated '), write(C), write(' clauses over '), write(N), write(' variables. '),nl,
        shell('cat header clauses > infile.cnf',_),
        write('Calling solver....'), nl,
        shell('picosat -v -o model infile.cnf', Result),  % if sat: Result=10; if unsat: Result=20.
        treatResult(Result),!.

treatResult(20):- write('Unsatisfiable'), nl, halt.
treatResult(10):- write('Solution found: '), nl, see(model), symbolicModel(M), seen, displaySol(M), nl,nl,halt.
treatResult( _):- write('cnf input error. Wrote anything strange in your cnf?'), nl,nl, halt.


initClauseGeneration:-  %initialize all info about variables and clauses:
        retractall(numClauses(   _)),
        retractall(numVars(      _)),
        retractall(varNumber(_,_,_)),
        assert(numClauses( 0 )),
        assert(numVars(    0 )),     !.

writeClause([]):- symbolicOutput(1),!, nl.
writeClause([]):- countClause, write(0), nl.
writeClause([Lit|C]):- w(Lit), writeClause(C),!.
w(-Var):- symbolicOutput(1), satVariable(Var), write(-Var), write(' '),!.
w( Var):- symbolicOutput(1), satVariable(Var), write( Var), write(' '),!.
w(-Var):- satVariable(Var),  var2num(Var,N),   write(-), write(N), write(' '),!.
w( Var):- satVariable(Var),  var2num(Var,N),             write(N), write(' '),!.
w( Lit):- told, write('ERROR: generating clause with undeclared variable in literal '), write(Lit), nl,nl, halt.


% given the symbolic variable V, find its variable number N in the SAT solver:
:-dynamic(varNumber / 3).
var2num(V,N):- hash_term(V,Key), existsOrCreate(V,Key,N),!.
existsOrCreate(V,Key,N):- varNumber(Key,V,N),!.                            % V already existed with num N
existsOrCreate(V,Key,N):- newVarNumber(N), assert(varNumber(Key,V,N)), !.  % otherwise, introduce new N for V

writeHeader:- numVars(N),numClauses(C), write('p cnf '),write(N), write(' '),write(C),nl.

countClause:-     retract( numClauses(N0) ), N is N0+1, assert( numClauses(N) ),!.
newVarNumber(N):- retract( numVars(   N0) ), N is N0+1, assert(    numVars(N) ),!.

% Getting the symbolic model M from the output file:
symbolicModel(M):- get_code(Char), readWord(Char,W), symbolicModel(M1), addIfPositiveInt(W,M1,M),!.
symbolicModel([]).
addIfPositiveInt(W,L,[Var|L]):- W = [C|_], between(48,57,C), number_codes(N,W), N>0, varNumber(_,Var,N),!.
addIfPositiveInt(_,L,L).
readWord( 99,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!. % skip line starting w/ c
readWord(115,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!. % skip line starting w/ s
readWord(-1,_):-!, fail. %end of file
readWord(C,[]):- member(C,[10,32]), !. % newline or white space marks end of word
readWord(Char,[Char|W]):- get_code(Char1), readWord(Char1,W), !.
%========================================================================================
