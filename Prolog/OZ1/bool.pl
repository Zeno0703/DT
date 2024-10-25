eval(X, tru) :- X = tru.
eval(X, fal) :- X = fal.

eval(and(X, Y), tru) :- eval(X, tru), eval(Y, tru).
eval(and(X, Y), fal) :- eval(X, tru), eval(Y, fal).
eval(and(X, Y), fal) :- eval(X, fal), eval(Y, tru).
eval(and(X, Y), fal) :- eval(X, fal), eval(Y, fal).

eval(or(X, Y), tru) :- eval(X, tru), eval(Y, tru).
eval(or(X, Y), tru) :- eval(X, tru), eval(Y, fal).
eval(or(X, Y), tru) :- eval(X, fal), eval(Y, tru).
eval(or(X, Y), fal) :- eval(X, fal), eval(Y, fal).

eval(not(X), fal) :- eval(X, tru).
eval(not(X), tru) :- eval(X, fal).