%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% To use this prolog template for other optimization problems, replace the code parts 1,2,3,4 below. %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% We have a factory of concrete products (beams, walls, roofs) that
%% works permanently (168h/week).  Every week we plan our production
%% tasks for the following week.  For example, one task may be to produce
%% a concrete beam of a certain type, which takes 10 hours and requires
%% (always one single unit of) the following resources: platform, crane,
%% truck, mechanic, driver.  But there are only a limited amount of units
%% of each resource available. For example, we may have only 3 trucks.  We
%% have 168 hours (numbered from 1 to 168) for all tasks, but we want to
%% finish all tasks as soon as possible.

%%%%%%%%%%%%%%%%%%%%%%% Input. %%%%%%%

maxHour(168).

%% format:
%% task( taskID, Duration, ListOFResourcesUsed ).
%% resource( resourceID, NumUnitsAvailable ).
 :-include(easy152).       % simple input example file.
%:-include(hardMaybe97).   % for this example there is a solution of cost 97.  We think it is optimal.
%:-include(hardMaybe147).  % for this example there is a solution of cost 147. We think it is optimal.

%%%%%% Some helpful definitions to make the code cleaner:

task(T):-              task(T,_,_).
duration(T,D):-        task(T,D,_).
usesResource(T,R):-    task(T,_,L), member(R,L).

%%%%%%
lastHour(Time, Durantion, LastHour):- LastHour is Time - Duration + 1.
endHour(InitHour, Duration, EndHour):- EndHour is InitHour + Duration - 1.

symbolicOutput(0).  % set to 1 to see symbolic output only; 0 otherwise.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1.- Declare SAT variables to be used
satVariable( start(T,H) ):- task(T), integer(H).   % "task T starts at hour H"     (MANDATORY)
% more variables will be needed.... See displaySol!


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2. This predicate writeClauses(MaxCost) generates the clauses that guarantee that
% a solution with cost at most MaxCost is found

writeClauses(infinite):- !, maxHour(M), writeClauses(M),!.
writeClauses(MaxHours):-
    eachTaskStartsOnce(MaxHours),
    defineTH(MaxHours),
    limitResources(MaxHours),
    true,!.
writeClauses(_):- told, nl, write('writeClauses failed!'), nl,nl, halt.

eachTaskStartsOnce(MaxHours):- task(TaskID), duration(TaskID, Duration), Max is MaxHours + 1 - Duration, findall(start-TaskID-Hour, between(1, Max, Hour), Lits), exactly(1, Lits), fail.
eachTaskStartsOnce(_).

defineTH(MaxHours):- task(TaskID), duration(TaskID, Duration), Max is MaxHours + 1 - Duration, between(1, Max, InitialHour), Max is InitHour + Duration - 1, between(InitialHour, Min, Possi), writeClause([\+start-TaskID-InitialHour, th-TaskID-PossibleHours]),fail.
defineTH(_).

limitResources(MaxHours):- resourceUnits(ResourceID, NumUnitsAvailable), between(1, Max, H), findall(th-TaskID-H, (task(TaskID,_,ListOFResourcesUsed), member(ResourceID, ListOFResourcesUsed)), Res), atMost(NumUnitsAvailable, Res), fail.
limitResources(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3. This predicate displays a given solution M:

% No need to modify displaySol:
displaySol(_):- nl,nl,   write('    '),
    write('        10        20        30        40        50        60        70        80'),
    write('        90       100       110       120       130       140       150       160'),nl, write('    '),
    write('12345678901234567890123456789012345678901234567890123456789012345678901234567890'),
    write('1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678'),nl,fail.
displaySol(M):- task(T), writeNum2(T), member(start(T,H),M), duration(T,D),
		B is H-1, writeX(' ',B), writeX('x',D), nl, fail.
displaySol(M):- findall(T-H,member(start(T,H),M),L), sort(L,L1), write(startTimes(L1)), write('.'),  nl,nl,!.
displaySol(_).

writeX(_,0):-!.
writeX(X,N):- write(X), N1 is N-1, writeX(X,N1),!.

writeNum2(T):-T<10, write(' '), write(T), write(': '), !.
writeNum2(T):-                  write(T), write(': '), !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 4. This predicate computes the cost of a given solution M:
%
costOfThisSolution(M,Cost):- findall(End, ( member(start(T,H),M), duration(T,D), End is H+D-1), L),  max_list(L,Cost),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% No need to modify anything below this line:

main:-  symbolicOutput(1), !, writeClauses(50), halt.   % print the clauses in symbolic form and halt
main:-
    told, write('Looking for initial solution with arbitrary cost...'), nl,
    initClauseGeneration,
    tell(clauses), writeClauses(infinite), told,
    tell(header),  writeHeader,  told,
    numVars(N), numClauses(C),
    write('Generated '), write(C), write(' clauses over '), write(N), write(' variables. '),nl,
    shell('cat header clauses > infile.cnf',_),
    write('Launching picosat...'), nl,
    shell('picosat -v -o model infile.cnf', Result),  % if sat: Result=10; if unsat: Result=20.
    treatResult(Result,[]),!.

treatResult(20,[]       ):- write('No solution exists.'), nl, halt.
treatResult(20,BestModel):-
    nl,costOfThisSolution(BestModel,Cost), write('Unsatisfiable. So the optimal solution was this one with cost '),
    write(Cost), write(':'), nl, displaySol(BestModel), nl,nl,halt.
treatResult(10,_):- %   shell('cat model',_),
    nl,write('Solution found '), flush_output,
    see(model), symbolicModel(M), seen,
    costOfThisSolution(M,Cost),
    write('with cost '), write(Cost), nl,nl,
    displaySol(M),
    Cost1 is Cost-1,   nl,nl,nl,nl,nl,  write('Now looking for solution with cost '), write(Cost1), write('...'), nl,
    initClauseGeneration, tell(clauses), writeClauses(Cost1), told,
    tell(header),  writeHeader,  told,
    numVars(N),numClauses(C),
    write('Generated '), write(C), write(' clauses over '), write(N), write(' variables. '),nl,
    shell('cat header clauses > infile.cnf',_),
    write('Launching picosat...'), nl,
    shell('picosat -v -o model infile.cnf', Result),  % if sat: Result=10; if unsat: Result=20.
    treatResult(Result,M),!.
treatResult(_,_):- write('cnf input error. Wrote something strange in your cnf?'), nl,nl, halt.


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
:-dynamic(varNumber / 3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Express that Var is equivalent to the disjunction of Lits:
expressOr( Var, Lits) :- symbolicOutput(1), write( Var ), write(' <--> or('), write(Lits), write(')'), nl, !.
expressOr( Var, Lits ):- member(Lit,Lits), negate(Lit,NLit), writeClause([ NLit, Var ]), fail.
expressOr( Var, Lits ):- negate(Var,NVar), writeClause([ NVar | Lits ]),!.

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
