%1)
select_row(Puzzle, Index, Row) :-
    nth1(Index, Puzzle, Row).

select_column(Puzzle, Index, Col) :-
    findall(X, (nth1(_, Puzzle, Row), nth1(Index, Row, X)), Col).

%2)
get_constraints(List, Constraints) :-
    get_constraints(List, [], ConstraintsR),
    reverse(ConstraintsR, Constraints).

get_constraints([], Res, Res).
get_constraints([0 | Tail], List, Res) :-
    get_constraints(Tail, List, Res).

get_constraints([1 | Tail], List, Res) :-
    count_ones(Tail, TailCount),
    drop_tail(Tail, TailCount, NewTail),
    Constraint is TailCount + 1,
    get_constraints(NewTail, [Constraint | List], Res).

count_ones([0 | _], 0).
count_ones([1 | Tail], Count) :-
    count_ones(Tail, Count1),
    Count is 1 + Count1.

drop_tail(NewTail, 0, NewTail).
drop_tail([_ | Tail], Count, NewTail) :-
    Count1 is Count - 1,
    drop_tail(Tail, Count1, NewTail).

%3)
make_empty_board(ColNum, RowNum, Board) :-
    make_rows(ColNum, RowNum, [], Board).

make_rows(_, 0, Res, Res).
make_rows(Cols, Rows, List, Res) :-
    make_col(Cols, [], EmptyCol),
    Rows1 is Rows - 1,
    make_rows(Cols, Rows1, [EmptyCol | List], Res).

make_col(0, Res, Res).
make_col(Cols, List, Res) :-
    Cols1 is Cols - 1,
    make_col(Cols1, [_ | List], Res).

%4)
solve(puzzle(Cols, Rows, RCons, ColCons), Sol) :-
    make_empty_board(Cols, Rows, Board),
    solve(Puzzle, Board, Visited, Sol).

solve(_, [], _, []).

% Random matrices genereren op basis van de constraints, telkens de rijen en kolommen
% uithalen met de functies die we zelf gemaakt hebben en dan kijken of ze gelijk zijn
% aan de constraints die de puzzel oplegt.