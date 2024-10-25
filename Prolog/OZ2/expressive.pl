    
eval(int(X), _List, Value) :- Value is X.
eval(var(A), List, Value) :- member(pair(A, Value), List).
eval(plus(X, Y), List, Value) :- eval(X, List, V1), eval(Y, List, V2), Value is V1 + V2.
eval(times(X, Y), List, Value) :- eval(X, List, V1), eval(Y, List, V2), Value is V1 * V2.
eval(pow(X, Y), List, Value) :- eval(X, List, V1), eval(Y, List, V2), Value is V1 ** V2.
eval(min(E), List, Value) :- eval(E, List, V1), Value is -V1.