% For the next Catalan Computer Science Olympiad, we want to make the best possible
% team by selecting a set of students. For each student, we have the following information:
%
% 1.- Her ID
% 2.- The list of other students which she considers friends. Note that S1 could consider S2
%     as a friend, but the converse is not necessarily true.
% 3.- The programming topic she is most expert on.
%
% The goal is to form a team with exactly K students such that each one of them considers all other team members a friend
% and all chosen students haver different expertise topics.
%
% Complete the writeClauses part below to find out one possible team.


%%% input:
numMembersTeam(6).
numStudents(25).
% info( id, list_of_students_(s)he_considers_a_friend, topic )
info(1,[4,9,11,14,15,16,21,23,24],t6).
info(2,[4,9,12,15,16,19,23],t8).
info(3,[3,7,10,11,12,13,15,17,21,22,24],t3).
info(4,[1,7,8,13,15,23,24],t4).
info(5,[1,8,9,11,13,17,18,23,24],t6).
info(6,[1,2,3,5,6,8,9,16,18,20,22,23,24],t6).
info(7,[5,10,11,12,14,20,25],t3).
info(8,[4,9,12,20,21],t1).
info(9,[3,6,9,15,20,23,24],t8).
info(10,[2,4,5,7,8,10,11,13,15,16,21,24],t8).
info(11,[1,3,5,11,12,14,15,17,20,22,24],t2).
info(12,[3,6,8,10,11,12,14,15,17,18,22],t5).
info(13,[2,4,6,8,10,13,15,18,19,24,25],t4).
info(14,[2,8,11,12,19],t4).
info(15,[3,4,6,11,12,14,15,16,17,22],t6).
info(16,[1,3,4,5,7,10,14,18,22,25],t3).
info(17,[2,3,9,11,12,14,15,16,17,19,20,22],t4).
info(18,[4,5,7,10,11,14,17,18,20,24],t8).
info(19,[3,4,7,19,24],t4).
info(20,[1,2,3,6,11,13,19],t8).
info(21,[1,10,17,19,23,24,25],t8).
info(22,[3,4,11,12,13,15,16,17,20,22],t1).
info(23,[6,11,21,24],t4).
info(24,[2,6,12,13,15,18,19,20,21,23,25],t7).
info(25,[1,3,8,10,13,18,20,21,24],t3).
%%% end input


%Helpful prolog predicates:
student(S):-     numStudents(N), between(1,N,S).
friends(S1,S2):- student(S1), student(S2), info(S1,L,_), member(S2,L).
expertise(S,E):- student(S), info(S,_,E).


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

satVariable( chosen(S) ):- student(S).   % chosen(S) means "student S has been chosen"


%%%%%%  2. Clause generation for the SAT solver:

writeClauses:- 
	grupEstudiants,
	totsFriends,
	totsExpDiferents,
	true.
writeClauses:- told, nl, write('writeClauses failed!'), nl,nl, halt.

grupEstudiants :- findall(chosen(S), student(S), List), numMembersTeam(K), exactly(K, List), fail.
grupEstudiants.

totsExpDiferents :- student(S), student(K), S\=K, expertise(S, E), expertise(K, E), writeClause([-chosen(K), -chosen(S)]), fail.
totsExpDiferents.

totsFriends :- student(S), student(K), S \= K, not(friends(S, K)), writeClause([-chosen(K), -chosen(S)]), fail.
totsFriends.

%%%%%%  3. DisplaySol: show the solution. Here M contains the literals that are true in the model:

%displaySol(M):- nl, write(M), nl, nl, fail.
displaySol(M):- write('Team: '), nl, student(S), member(chosen(S), M), expertise(S,E), write(S-E), nl, fail.
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
