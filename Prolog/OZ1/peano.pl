min(X, zero, X).
min(s(X), s(Y), Z) :- min(X, Y, Z).
% verandert constant in een lagere, dus bv (3, 2, 1), gaat naar (2, 1, 1) naar (1, 0, 1)

greater_than(s(X), zero).
greater_than(s(X), s(Y)) :- greater_than(X, Y).
% verandert naar een lagere X en Y tot eentje zero is

maximum(X, Y, X) :- greater_than(X, Y).
maximum(X, Y, Y) :- greater_than(Y, X).
maximum(zero, zero, zero).
% dit doen met s(s(s(s(zero)))) voor 4 bv.

maximum(X, zero, X).
maximum(zero, Y, Y).
maximum(zero, zero, zero).
maximum(s(X), s(Y), s(Z)) :- maximum(X, Y, Z).

peano_plus(zero,X,X).
peano_plus(s(X),Y,s(Z)) :- peano_plus(X,Y,Z).