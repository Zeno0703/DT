highway(1,2,yellow).
highway(2,3,blue).
highway(1,3,yellow).

check :- 
    findall(X, highway(X, _, _), Set1),
    findall(X, highway(_, X, _), Set2),
    append(Set1, Set2, Set),
    sort(Set, Nodes),
    check(Nodes).

check([]).
check([N|Tail]) :- 
    check_even_at(N),
    check_colors_at(N),
    check(Tail).

check_even_at(N) :- 
    findall(X, (highway(X, N, _) ; highway(N, X, _)), Neigh),
    sort(Neigh, Neighb),
    length(Neighb, Y),
    Y mod 2 =:= 0.

check_colors_at(1) :- !.
check_colors_at(N) :- 
    findall(C, (highway(N, _, C), highway(_, N, C)), Colours),
    length(Colours, Clen),
    \+ (
        member(C1, Colours),
        findall(C2, (C2 = C1, member(C2, Colours)), Matches),
        length(Matches, MatchLength),
        MatchLength * 2 > Clen
    ).

tour(T) :-
    check,
    path(1, nil, [], T).

path(1, _, Acc, Res) :-
    findall(highway(S, E, C), highway(S, E, C), Allhighways),
    length(Allhighways, HighwayCount),
    length(Acc, PathLenth),
    HighwayCount =:= PathLenth,
    Res = Acc.

path(Node, PrevColor, Acc, Res) :-
    findall(highway(Node, E, C), (highway(Node, E, C), C\=PrevColor, \+ member(highway(Node, E, C), Acc)), PossibleHighways1),
    findall(highway(S, Node, C), (highway(S, Node, C), C\=PrevColor, \+ member(highway(S, Node, C), Acc)), PossibleHighways2),
    append(PossibleHighways1, PossibleHighways2, PossibleHighways),
    member(Next, PossibleHighways),
    (Next = highway(Node, NextNode, Color); Next = highway(NextNode, Node, Color)),
    path(NextNode, Color, [Next|Acc], Res).