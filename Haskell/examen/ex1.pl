move(left, tape([#], Head, R), tape([], #, [Head|R])).
move(left, tape([L1|L], Head, R), tape(L, L1, [Head|R])).
move(right, tape(L, Head, [#]), tape([Head|L], #, [])).
move(right, tape(L, Head, [R1|R]), tape([Head|L], R1, R)).

read_tape(tape(_, Head, _), Head).

write_tape(tape(L, _, R), X, tape(L, X, R)).