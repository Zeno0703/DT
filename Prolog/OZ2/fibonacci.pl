fib(1, 0).
fib(2, 1).
fib(N, X) :- N > 2, MIN1 is N - 1, MIN2 is N - 2, fib(MIN1, Y), fib(MIN2, Z), X is Y + Z.

fib2(N, FN) :- fibHelper(N, FN, [1, 0]).

fibHelper(1, FN, [_,FN]).
fibHelper(2, FN, [FN|_]).
fibHelper(N, FN, [Y1,Y2|T]) :- 
    N > 2, 
    MIN1 is N-1, 
    Y3 is Y1+Y2, 
    fibHelper(MIN1, FN, [Y3,Y1,Y2|T]).