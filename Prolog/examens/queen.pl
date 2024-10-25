%!esystant

queens(N, Board) :-
    numlist(1, N, List),
    permutation(List, Board),
    valid(Board).

valid([]).
valid([Head|Tail]) :- 
    (
      no_attacks(1, Head, Tail) -> valid(Tail)
      ;
      false  
    ).

no_attacks(_, _, []).
no_attacks(Dist, Head, [TailHead | Tail]) :-
    Yplus is Head + Dist, Ymin is Head - Dist,
    Dist1 is Dist + 1,
    (
        (Yplus == TailHead ; Ymin == TailHead) -> false
        ;
        no_attacks(Dist1, Head, Tail)
    ).