fib(1, 1).
fib(2, 1).
fib(N, F) :-
    N1 is N - 1,
    N2 is N - 2,
    fib(N1, F1),
    fib(N2, F2),
    F is F1 + F2.

fib2(N, F) :- fibHelper(N, F, [1, 0]).

fibHelper(1, F, [F | _]).
fibHelper(N, F, [Y1, Y2 | Tail]) :-
    N > 1,
    Y3 is Y1 + Y2,
    N1 is N - 1,
    fibHelper(N1, F, [Y3, Y1, Y2 | Tail]).