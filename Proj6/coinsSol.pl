:- use_module(library(clpfd)).

ejemplo(0,   26, [1,2,5,10] ).  % Solution: [1,0,1,2]
ejemplo(1,  361, [1,2,5,13,17,35,157]).

main:- 
    ejemplo(0,Amount,Coins),
    nl, write('Paying amount '), write(Amount), write(' using the minimal number of coins of values '), write(Coins), nl,nl,
    length(Coins,N), 
    length(Vars, N), % get list of N prolog vars    

%1: Dominio:
    Vars ins 0..Amount,   % en el peor de los casos, solo hay monedas de valor 1...

%2: Constraints:
    expr( Vars, Coins, Expr ),  % por ejemplo expr( [X1,X2,X3,X4], [1,2,5,10],   X4*10 + X3*5 + X2*2 + X1*1 )
    Expr #= Amount,
    exprSuma( Vars, ExprSum ),  % por ejemplo exprSuma( [X1,X2,X3,X4], X4+X3+X2+X1 )

%3: labeling:
    labeling( [min(ExprSum)], Vars  ),

%Escribimos el resultado:
    NumCoins is ExprSum,    write('We need '), write(NumCoins), write(' coins: '), write(ExprSum), nl,nl, halt.


% expr( Vars, Coins, Expr ): atención! esto no calcula nada! Solo genera una structura de datos prolog, que es una expresión
expr( [], [], 0 ).
expr( [X|Vars], [K|Coins], Expr + X*K ):- expr( Vars, Coins, Expr ).

exprSuma( [X],  X ):- !.
exprSuma( [X|Vars], X+Expr ):- exprSuma( Vars, Expr).
