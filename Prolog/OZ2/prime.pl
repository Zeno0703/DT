all_primes(N, List) :- numlist(2, N, All), 
                        sieve(All, List).

sieve([], []).
sieve([X|Rest], Primes) :- traverse(X, Rest, Sieved),
                            sieve(Sieved, [X|Primes]).

traverse(X, [], [_]).
traverse(X, [Y|Rest], Sieved) :-
    (Y mod X =:= 0
        -> traverse(X, Rest, Sieved)
        ;
        traverse(X, Rest, [Y|Sieved])
    ).