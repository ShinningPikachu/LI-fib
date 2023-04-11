% We want to solve a modified version of the Traveling Salesman Problem: given a map with
% some (possibly one-way) roads between N cities, we want find a route starting and
% finishing in city 1 such that each city is visited exactly once. Moreover, we want to
% find a route with cost at most maxCost, considering that:
%  -trajects between cities with the same parity have cost 0 euros
%  -trajects between cities of different parity  have cost 1 euro
% For example, the following solution has cost 10:
%   1 10 4 8 3 20 18 14 12 11 15 2 5 9 17 16 7 13 19 6 1 
%    ^      ^ ^           ^     ^ ^      ^  ^       ^ ^
% Complete the following program to compute one such a route.


%%% input:
maxCost(10).
numCities(20).
adjacency(1,[10]).
adjacency(2,[7,5,17]).  % Three one-way roads from city 2:  2->7, 2->5 and 2->17.
adjacency(3,[6,20,11]).
adjacency(4,[14,15,1,8,5]).
adjacency(5,[16,4,9,10,11]).
adjacency(6,[1,13,17]).
adjacency(7,[16,9,13,11]).
adjacency(8,[7,3,10,6,17]).
adjacency(9,[6,17,11]).
adjacency(10,[7,11,5,6,4]).
adjacency(11,[20,8,15,4,1,16,3]).
adjacency(12,[15,11,3]).
adjacency(13,[5,2,19,3,6]).
adjacency(14,[10,12,9,7]).
adjacency(15,[20,1,14,18,12,2]).
adjacency(16,[7,6,4,8,2,10]).
adjacency(17,[20,5,16,3,8]).
adjacency(18,[15,16,7,14,3]).
adjacency(19,[13,6]).
adjacency(20,[12,6,18,7,16]).
%%% end input


%Helpful prolog predicates:
position(P):- numCities(N), between(0,N,P).
city(I):-     adjacency(I,_).


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
satVariable( visited(I,P) ):-  city(I), position(P). % visited(I,P) meaning "city I is visited in position P"
% Warning: more types of variables might be needed or convenient.


%%%%%%  2. Clause generation for the SAT solver:

writeClauses:- 
    onlyCity,
    onlyPosition,
    numberOne,
    adjecent,
    paridad,
    true.
writeClauses:- told, nl, write('writeClauses failed!'), nl,nl, halt.

onlyCity :- city(I), I \= 1, findall(visited(I, P) ,position(P), List), exactly(1, List), fail.
onlyCity.

onlyPosition :- position(P), findall(visited(I, P), city(I), List), exactly(1, List), fail.
onlyPosition.

numberOne :- numCities(N), writeClause([visited(1, 0)]), writeClause([visited(1, N)]), fail.
numberOne.

adjecent :- numCities(N), city(I), city(I1), position(P), P1 is P + 1, P \= -1, P \= N, adjacency(I, L), not(member(I1, L)), writeClause([-visited(I, P), -visited(I1, P1)]), fail.
adjecent.

changeParidad(P):- city(I1), city(I2), visited(I1, P),P1 is P+1, visited(I2, P1), L1 is I1 mod 2, L2 is I2 mod 2, L1 = L2.

paridad :- maxCost(M), numCities(N), city(I), findall(visited(I, P), position(P), List), atMost(M, [changeParidad(0),...,changeParidad(N)]), fail.
paridad.

%%%%%%  3. DisplaySol: show the solution. Here M contains the literals that are true in the model:

%displaySol(M):- nl, write(M), nl, nl, fail.
displaySol(M):- position(P), member(visited(City,P),M), write(City), write(' '), fail.
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
