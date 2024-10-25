%%%%%%%%%%%%%%%%%%
%% ASSIGNMENT 1 %%
%%%%%%%%%%%%%%%%%%

box_contains(box(_, (X1, Y1), (X2, Y2)), (X, Y)) :- 
    between(X1, X2, X),
    between(Y1, Y2, Y).

box_at(puzzle(_, _, Boxes), Pos) :-
    findall(Box, (member(Box, Boxes), box_contains(Box, Pos)), BoxAtPosition),
    length(BoxAtPosition, N),
    N >= 1.

%%%%%%%%%%%%%%%%%%
%% ASSIGNMENT 2 %%
%%%%%%%%%%%%%%%%%%

move_box(puzzle(X, Y, Boxes), Box, Dir, NewPuzzle) :- 
    get_orientation(Box, Orientation),
    get_direction(puzzle(X, Y, Boxes), Box, Orientation, Dir),
    move_direction(Box, Dir, NewBox),
    select(Box, Boxes, BoxesWO),
    NewBoxes = [NewBox | BoxesWO],
    sort_boxes(NewBoxes, Sorted),
    NewPuzzle = puzzle(X, Y, Sorted).

compare_boxes(Result, box(Name1, _, _), box(Name2, _, _)) :-
    compare(Result, Name1, Name2).

sort_boxes(Boxes, SortedBoxes) :-
    predsort(compare_boxes, Boxes, SortedBoxes).

get_orientation(box(_, (X1, Y1), (X2, Y2)), Or) :-
    (
        X1 == X2 -> Or = 1 % Verticaal
        ;
        Y1 == Y2 -> Or = 0 % Horizontaal   
    ).

get_direction(Puzzle, Box, 1, Direction) :-
    (
        check_intersection(Puzzle, Box, up) -> Direction = up
        ;
        check_intersection(Puzzle, Box, down) -> Direction = down
        ;
        fail
    ).

get_direction(Puzzle, Box, 0, Direction) :-
    (
        check_intersection(Puzzle, Box, right) -> Direction = right
        ;
        check_intersection(Puzzle, Box, left) -> Direction = left
        ;
        fail
    ).

check_intersection(puzzle(_, _, Boxes), box(_, (_, _), (X2, Y2)), up) :-
    findall(Box, (member(Box, Boxes), Yplus is Y2 + 1, box_contains(Box, (X2, Yplus))), Intersect),
    length(Intersect, N),
    N == 0.

check_intersection(puzzle(_, _, Boxes), box(_, (X1, Y1), (_, _)), down) :-
    findall(Box, (member(Box, Boxes), Ymin is Y1 - 1, box_contains(Box, (X1, Ymin))), Intersect),
    length(Intersect, N),
    N == 0.

check_intersection(puzzle(_, _, Boxes), box(_, (_, _), (X2, Y2)), right) :-
    findall(Box, (member(Box, Boxes), Xplus is X2 + 1, box_contains(Box, (Xplus, Y2))), Intersect),
    length(Intersect, N),
    N == 0.

check_intersection(puzzle(_, _, Boxes), box(_, (X1, Y1), (_, _)), left) :-
    findall(Box, (member(Box, Boxes), Xmin is X1 - 1, box_contains(Box, (Xmin, Y1))), Intersect),
    length(Intersect, N),
    N == 0.

move_direction(box(C, (X1, Y1), (X2, Y2)), up, box(C, (X1, NY1), (X2, NY2))) :-
    NY1 is Y1 + 1, NY2 is Y2 + 1.

move_direction(box(C, (X1, Y1), (X2, Y2)), down, box(C, (X1, NY1), (X2, NY2))) :-
    NY1 is Y1 - 1, NY2 is Y2 - 1.

move_direction(box(C, (X1, Y1), (X2, Y2)), right, box(C, (NX1, Y1), (NX2, Y2))) :-
    NX1 is X1 + 1, NX2 is X2 + 1.

move_direction(box(C, (X1, Y1), (X2, Y2)), left, box(C, (NX1, Y1), (NX2, Y2))) :-
    NX1 is X1 - 1, NX2 is X2 - 1.

    
%%%%%%%%%%%%%%%%%%
%% ASSIGNMENT 3 %%
%%%%%%%%%%%%%%%%%%

finished(puzzle(X, _, Boxes)) :- 
    member(box(red, (_, _), (X2, _)), Boxes),
    X2 >= X.

finished(puzzle(X, Y, Boxes)) :-
    member(box(red, (X1, Y1), (X2, Y2)), Boxes),
    move_box(puzzle(X, Y, Boxes), box(red, (X1, Y1), (X2, Y2)), right, NewPuzzle),
    finished(NewPuzzle).

%%%%%%%%%%%%%%%%%%
%% ASSIGNMENT 4 %%
%%%%%%%%%%%%%%%%%%

solve(Puzzle, Depth, Sol) :- 
    solve(Puzzle, Depth, [], Sol).

solve(_, Max, _, _) :- 
    Max < 0, !,
    fail.

solve(Puzzle, _, _, []) :-
    finished(Puzzle).

solve(Puzzle, MaxDepth, Visited, [ID-Dir | Tail]) :-
    move_box(Puzzle, box(ID, _, _), Dir, NewPuzzle),
    \+ member(NewPuzzle, Visited),
    MaxDepth1 is MaxDepth - 1,
    solve(NewPuzzle, MaxDepth1, [Puzzle | Visited], [Tail]).


%%%%%%%%%%%%%%%%%%
%% PUZZLES      %%
%%%%%%%%%%%%%%%%%%

puzzle(1, P) :-
    P = puzzle(6, 6, 
            [box(blue,(1,1),(2,1)), box(dark,(3,2),(3,4)),
             box(green,(1,5),(2,5)), box(orange,(0,0),(0,1)),
             box(purple,(0,3),(0,5)), box(red,(1,3),(2,3)),
             box(silver,(2,0),(4,0)), box(yellow,(5,0),(5,2))]).

puzzle(2, P) :-
    P = puzzle(6, 6,
            [box(green,(2,4),(2,5)), box(yellow,(3,5),(5,5)),
             box(orange,(4,3),(4,4)), box(purple,(4,0),(4,2)),
             box(red,(2,3),(3,3))]).
