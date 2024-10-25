%!esystant
depth(nil, 0).
depth(t(L, _, R), D) :-
    depth(L, DL),
    depth(R, DR),
    D is (max(DL, DR) + 1).

balanced(nil).
balanced(t(L, _, R)) :-
    depth(L, DL),
    depth(R, DR),
    Diff is DR - DL,
    abs(Diff) =< 1,
    balanced(L),
    balanced(R).

add_to(nil, Val, t(nil, Val, nil)).
add_to(t(L, X, R), Val, t(NL, X, R)) :-
    depth(L, DL), depth(R, DR), DL =< DR,!,
    add_to(L, Val, NL).

add_to(t(L, X, R), Val, t(L, X, NR)) :-
    add_to(R, Val, NR).
