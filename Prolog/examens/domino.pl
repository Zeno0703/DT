get_col(Matrix, Index, Col) :-
    findall(Val, (nth1(_, Matrix, Row), nth1(Index, Row, Val)), Col).

sTranspose(Matrix, Transposed) :-
    nth0(0, Matrix, Row),
    length(Row, N),
    generateRows(Matrix, N, [], Transposed).

generateRows(_, 0, Res, Res).
generateRows(Matrix, ColNum, List, Res) :-
    findall(X, (nth1(_, Matrix, Row), nth1(ColNum, Row, X)), Col),
    ColNum1 is ColNum - 1,
    generateRows(Matrix, ColNum1, [Col | List], Res).

get_col2(Matrix, Index, Col) :-
    sTranspose(Matrix, Transposed),
    nth1(Index, Transposed, Col).