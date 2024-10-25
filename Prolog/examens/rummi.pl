%%%%%%%%%%%%%%%%%%
%% ASSIGNMENT 1 %%
%%%%%%%%%%%%%%%%%%

valid_table(Table) :- 
    findall(List1, member(crow(List1), Table), NumberList),
    findall(List2, member(nrow(List2), Table), ColourList),
    checkNumberList(NumberList),
    checkColourList(ColourList).

checkNumberList([]).
checkNumberList([Head|Tail]):-
    checkNumberRow(Head),
    checkNumberList(Tail).

checkNumberRow(List) :-
    length(List, L), L >= 3,
    findall(N, member(block(N, _), List), Numbers),
    findall(C, member(block(_, C), List), Colours),
    list_to_set(Numbers, Set1), length(Set1, L1), L1 == 1,
    list_to_set(Colours, Set2), length(Set2, L2), L == L2.

checkColourList([]).
checkColourList([Head|Tail]):-
    checkColourRow(Head),
    checkColourList(Tail).

checkColourRow(List) :-
    length(List, L), L >= 3,
    findall(N, member(block(N, _), List), Numbers),
    findall(C, member(block(_, C), List), Colours),
    list_to_set(Colours, Set1), length(Set1, L1), L1 == 1,
    list_to_set(Numbers, Set2), length(Set2, L2), L == L2,
    stair(Set2).

stair(List) :- length(List, N) , N == 1.
stair([X1, X2 | Tail]) :-
    Xplus is X1 + 1,
    Xplus =:= X2,
    stair([X2 | Tail]).

%%%%%%%%%%%%%%%%%%
%% ASSIGNMENT 2 %%
%%%%%%%%%%%%%%%%%%

play_game(_P1,_P2,_Table,_Bag) :- fail.

