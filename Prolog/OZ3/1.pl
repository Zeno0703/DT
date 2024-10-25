queens(N, L) :- numlist(1, N, List),
                permutation(List, L),
                \+ attack_possible(L).

attack_possible([X|Tail]) :- check_diag(X, Tail, 1),
                            attack_possible(Tail).

check_diag(X, [Y|Rest], DeltaX) :- 
        (DeltaY is abs(X - Y), DeltaX =:= DeltaY)
        ;
        (NewX is DeltaX + 1, check_diag(X, Rest, NewX)).