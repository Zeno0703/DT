%tape(Left, Symbol, Right)

move(left,tape([],Symbol,Rs),tape([],#,[Symbol|Rs])).
move(left,tape([L|Ls],Symbol,Rs),tape(Ls,L,[Symbol|Rs])).
move(right,tape(Ls,Symbol,[]),tape([Symbol|Ls],#,[])).
move(right,tape(Ls,Symbol,[R|Rs]),tape([Symbol|Ls],R,Rs)).

read_tape(tape(_,Symbol,_),Symbol).

write_tape(Symbol,tape(L,_,R),tape(L,Symbol,R)).