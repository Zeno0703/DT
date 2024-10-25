%!esystant

alpha_beta(leaf(S,V), _, _, S, leaf(S,V)).
alpha_beta(max(L, R), A, B, S, max(NL, NR)):-
    alpha_beta(L, A, B, Score, NL),
    Alpha is max(A, Score),
    ((Alpha >= B, NR = nil, S is Score);
    (Alpha < B, alpha_beta(R, Alpha, B, Score2, NR), S is max(Score, Score2))).
alpha_beta(min(L, R), A, B, S, min(NL, NR)):-
    alpha_beta(L, A, B, Score, NL),
    Beta is min(B, Score),
    ((A >= Beta, NR = nil, S is Score);
    (A < Beta, alpha_beta(R, A, Beta, Score2, NR), S is min(Score, Score2))).

% alpha_beta(max(min(max(leaf(1,who),leaf(3,knew)),max(leaf(5,prolog),leaf(1,was))),min(max(leaf(2,such),leaf(2,fun)),max(leaf(5,!),leaf(5,!)))),-10,10,S,T).