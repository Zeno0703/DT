all_primes(N , Primes) :-
    numlist(2, N, List),
    sieve(2, N, List, Primes).

sieve(Max, Max, Res, Res).
sieve(Count, Max, List, Res) :-
    Count1 is Count + 1,
    filter_co_prime(Count, List, [], CoPrimes),
    reverse(CoPrimes, RPrimes),
    sieve(Count1, Max, RPrimes, Res).

filter_co_prime(_, [], Res, Res).
filter_co_prime(Count, [Head | Tail], Helper, Res) :-
    (
      is_co_prime(Head, Count) -> filter_co_prime(Count, Tail, [Head | Helper], Res)
      ;
        filter_co_prime(Count, Tail, Helper, Res)
    ).

is_co_prime(X, N) :-
    X == N;
    (Y is X mod N, Y \= 0).