%% Name          : TODO
%% Student number: TODO

%%%%%%%%%%%%%%%%%%%%
%%% Assignment 1 %%%
%%%%%%%%%%%%%%%%%%%%
maximalChains(board(_, Constraints), Chains):-
    findall([Pos1, Pos2], member(gt(Pos1, Pos2), Constraints), Greaters),
    length(Greaters, N),
    getAllChains(Greaters, N, [], AllChains),
    flatRemDup(AllChains, [], ChainsR),
    reverse(ChainsR, ChainsX),
    length(ChainsX, LN),
    removeSublists(ChainsX, LN, [], ChainsY),
    findall(chain( List ), member(List, ChainsY), Chains).

getAllChains(_, 0, Res, Res).
getAllChains(Greaters, Count, List, Res) :-
    nth1(Count, Greaters, Elem), select(Elem, Greaters, Rest),
    getChain(Elem, Rest, [], SubChain),
    Count1 is Count - 1,
    getAllChains(Greaters, Count1, [SubChain | List], Res).

getChain([], _, Res, Res).
getChain([Pos1, Pos2], Rest, List, Res) :-
    NewList = [[Pos1, Pos2] | List],
    findall([Pos2, PosX], member([Pos2, PosX], Rest), Link),
    length(Link, L),
    (
        (L \= 0) ->     (nth0(0, Link, NewLink),
                        select(NewLink, Rest, NewRest),
                        getChain(NewLink, NewRest, NewList, Res))
        ;
        getChain([], Rest, NewList, Res)
    ).

flatRemDup([], Res, Res).
flatRemDup([Head | Tail], List, Res) :-
    flatten(Head, FlatHead),
    sort(FlatHead, Sorted),
    flatRemDup(Tail, [Sorted | List], Res).

removeSublists(_, 0, Res, Res).
removeSublists(All, Count, List, Res) :-
    nth1(Count, All, Elem), select(Elem, All, Rest),
    Count1 is Count - 1,
    (
        (isNoSublist(Elem, Rest)) -> removeSublists(All, Count1, [Elem | List], Res)
        ;
    removeSublists(All, Count1, List, Res)
    ).

isNoSublist(_, []).
isNoSublist(Elem, [Head | Tail]) :-
    (
        isSublist(Elem, Head) -> false
        ;
        isNoSublist(Elem, Tail)
    ).

isSublist(List1, List2) :- forall(member(Elem, List1), member(Elem, List2)).

%%%%%%%%%%%%%%%%%%%%
%%% Assignment 2 %%%
%%%%%%%%%%%%%%%%%%%%
mainarizumu(_, _):-
  fail.

%%%%%%%%%%%%%%%%%%%%
%%% Assignment 3 %%%
%%%%%%%%%%%%%%%%%%%%
island(_, _, _):-
  fail.
