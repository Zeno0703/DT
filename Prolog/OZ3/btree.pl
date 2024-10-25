%!esystant
    
depth(nil, 0).
depth(t(nil, _, nil), 1).
depth(t(L, _, R), Depth):-
    depth(L, DepthL),
    depth(R, DepthR),
    Depth is max(DepthL, DepthR) + 1.

balanced(nil).
balanced(t(L, _, R)) :- 
    depth(L, DL),
    depth(R, DR),
    abs(DL - DR) =< 1,
    balanced(L),
    balanced(R).
    
add_to(nil, N, t(nil, N, nil)).
add_to(t(L, X, R), N, t(LN, X, R)) :- 
    depth(L, DL),
    depth(R, DR),
    DR >= DL,
    add_to(L, N, LN).

add_to(t(L, X, R), N, t(L, X, RN)) :- 
    depth(L, DL),
    depth(R, DR),
    DL >= DR,
    add_to(R, N, RN).