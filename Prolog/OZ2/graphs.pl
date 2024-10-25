edge(a, b).
edge(b, d).
edge(b, c).
edge(d, c).
node(a).
node(b).
node(c).
node(d).
node(e).


neighbor(X,Y) :- edge(X, Y); edge(Y, X).

path(X,Y) :- neighbor(X, Y).
path(X,Y) :- neighbor(X, Z), path(Z, Y).

path2(X,Y) :- pathHelper(X,Y,[X]).

%pathHelper(X,Y,Visited).
pathHelper(X, Y, Visited) :- neighbor(X, Y), \+ member(Y, Visited).
pathHelper(X, Y, Visited) :- neighbor(X, Z), \+ member(Z, Visited), pathHelper(Z, Y, [Z|Visited]).