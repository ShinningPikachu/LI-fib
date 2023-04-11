%%%%%%% =======================================================================================
%
% miSudoku.pl:  simple example of our LI Prolog method for solving problems using a SAT solver.
% 
% It generates the SAT clauses, calls the SAT solver, and shows the solution. Just specify:
%       1. SAT Variables
%       2. Clause generation
%       3. DisplaySol: show the solution.
%
%%%%%%% =======================================================================================


symbolicOutput(0).  % set to 1 for debugging: to see symbolic output only; 0 otherwise.


entrada([ [-,4,-,  -,-,-,  -,1,-],      %   Solution shown      6 4 3  9 7 5  2 1 8       
          [-,-,8,  -,3,-,  9,-,-],      %      for this         2 7 8  4 3 1  9 5 6       
          [-,-,-,  6,8,2,  -,-,-],      %   input example:      5 1 9  6 8 2  3 4 7       
                                        %                 
          [3,2,-,  -,6,-,  -,7,9],      %                       3 2 5  8 6 4  1 7 9       
          [-,-,7,  -,-,-,  4,-,-],      %                       1 8 7  3 5 9  4 6 2       
          [9,6,-,  -,1,-,  -,8,3],      %                       9 6 4  2 1 7  5 8 3       
                                        %                                      
          [-,-,-,  7,9,8,  -,-,-],      %                       4 5 2  7 9 8  6 3 1       
          [-,-,1,  -,2,-,  7,-,-],      %                       8 3 1  5 2 6  7 9 4       
          [-,9,-,  -,-,-,  -,2,-] ]).   %                       7 9 6  1 4 3  8 2 5       

%%%%%% Some helpful definitions to make the code cleaner:
row(I):-between(1,9,I).
col(J):-between(1,9,J).
val(K):-between(1,9,K).

blockID(Iid,Jid):- member(Iid,[0,1,2]), member(Jid,[0,1,2]).   %there are 9 blocks: 0-0 1-0 ...2-2
squareOfBlock( Iid,Jid, I,J ):- row(I), col(J), Iid is (I-1) // 3,  Jid is (J-1) // 3.


%%%%%%  1. SAT Variables:

% x(I,J,K) means "square IJ gets value K",    1<=i<=9, 1<=j<=9, 1<=k<=9   9^3= 729 variables
satVariable( x(I,J,K) ):- row(I), col(J), val(K).

%%%%%%  2. Clause generation for the SAT solver:

writeClauses:- 
    filledInputValues,         % for each filled-in value of the input, add a unit clause
    eachIJexactlyOneK,         % each square IJ gets exactly one value K
    eachJKexactlyOneI,         % each column J gets each value K in exactly one row I
    eachIKexactlyOneJ,         % each row    I gets each value K in exactly one column J
    eachBlockEachKexactlyOnce, % each 3x3 block gets each value K exactly once.
    true,!.                    % this way you can comment out ANY previous line of writeClauses
writeClauses:- told, nl, write('writeClauses failed!'), nl,nl, halt.

%% We can use writeClause to write any kind of clauses for the SAT solver, any list of positive and 
%%     negative literals, such as:  writeClause([ -x(1,2,3),  x(2,2,3),  x(4,2,3), -x(5,5,6) ]).
%% We can also generate *constraints* for the SAT solver: exactly(K,Lits), atLeast(K,Lits), atMost(K,Lits).
%%     Look at the library below to see how these constraints generate the necessary SAT clauses to encode them.
%%     For example, exactly 1 of {x1,x2,...x9} is equivalent to:
%%           - at least 1 of {x1,x2,...x9} can be encoded by a single clause: x1 v...v x9
%%           - at most  1 of {x1,x2,...x9} by 36 binary clauses:   -x1 v -x2,   -x1 v -x3,   ...   -x8 v -x9.


%% The Prolog predicate nth1(I,L,E)  means:   "the Ith element of the list L is E"
%% Please understand the Prolog mechanism we use for generating all clauses: one line with "fail", and another one below:
filledInputValues:- entrada(Sud), nth1(I,Sud,Row), nth1(J,Row,K), integer(K), writeClause([ x(I,J,K) ]), fail.
filledInputValues.

%% The Prolog predicate  findall(X, Cond, L)   means:   "L = { X | Cond } "
eachIJexactlyOneK:- row(I), col(J), findall( x(I,J,K), val(K), Lits ), exactly(1,Lits), fail.
eachIJexactlyOneK.
%% this generates, for example,  exactly( 1, [ x(1,1,1), x(1,1,2), ... x(1,1,9) ])

eachJKexactlyOneI:- col(J), val(K), findall( x(I,J,K), row(I), Lits ), exactly(1,Lits), fail.
eachJKexactlyOneI.

eachIKexactlyOneJ:- row(I), val(K), findall( x(I,J,K), col(J), Lits ), exactly(1,Lits), fail.
eachIKexactlyOneJ.

eachBlockEachKexactlyOnce:- blockID(Iid,Jid), 
        val(K), findall( x(I,J,K), squareOfBlock(Iid,Jid,I,J), Lits ), exactly(1,Lits), fail.
eachBlockEachKexactlyOnce.


%%%%%%  3. DisplaySol: show the solution. Here M contains the literals that are true in the model:

%displaySol(M):- nl, write(M), nl, nl, fail.
displaySol(M):- nl, row(I), nl, line(I), col(J), space(J), member(x(I,J,K), M ), write(K), write(' '), fail.
displaySol(_):- nl,nl.

line(I):-member(I,[4,7]), nl,!.
line(_).
space(J):-member(J,[4,7]), write(' '),!.
space(_).

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
