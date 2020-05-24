%prime(N) - проверяет,что N простое число
inc(N, R) :- number(N), !, R is N + 1.
inc(N, R) :- number(R), !, N is R - 1.
divisible(X, Y) :- X \= Y, N is Y * Y, N =< X, 0 is mod(X, Y).
divisible(X, Y) :- X \= Y, inc(Y, Y1), divisible(X, Y1).
prime(N) :- N > 1, Y is 2, \+divisible(N, Y).

%composite(N) - проверяет, что N - составное.
composite(N) :- N > 1, \+prime(N).

%prime_divisors - проверяющее, что список Divisors содержит все простые делители числа N, упорядоченные по возрастанию. Если N делится на простое число P несколько раз, то Divisors должен содержать соответствующее число копий P.
prime_divisors(N, []) :- N is 1.
prime_divisors(N, [H | T]) :- 0 is mod(N, H), N1 is div(N, H), prime_divisors(N1, T).