%!esystant

queens(N,L) :-
    numlist(1,N,List),
    permutation(List, L),
    \+ attack_possible(L).

attack_possible([X|Tail]) :-
    % Check voor X of hij rechts iets raakt
    check_right(X, Tail, 1);
    attack_possible(Tail).

check_right(X, [Y|Tail], DeltaX) :- 
    %deltaX, deltaY abs()
    (DeltaY is X - Y,
    attacks(DeltaX, DeltaY))
    ;
    (NewX is DeltaX = 1,
    check_right(X, ))
