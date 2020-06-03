% HARD
init(MAX_N) :- sieve_of_Eratosthenes(2, MAX_N).

inc(N, R) :- number(N), !, R is N + 1.
inc(N, R) :- number(R), !, N is R - 1.

%prime(N) - проверяет,что N простое число
prime(N) :- \+ compositeTable(N), !.

make_composite(N, J, MAX_N) :- N =< MAX_N, assert(compositeTable(N)), NN is N + J, make_composite(NN, J, MAX_N).
sieve_of_Eratosthenes(N, MAX_N) :-
	K is N * N,
	K =< MAX_N,
	\+compositeTable(N),
	NN is 2 * N,
	make_composite(NN, N, MAX_N).
sieve_of_Eratosthenes(N, MAX_N) :-
	N =< MAX_N,
	inc(N,R),
	sieve_of_Eratosthenes(R, MAX_N).
%composite(N) - проверяет, что N - составное.
composite(N) :- compositeTable(N), !.

%next_prime(N, Result) - следующее простое.
next_prime(N, A) :- inc(N, R), prime(R), !, A is R.
next_prime(N, A) :- inc(N, R), \+prime(R), next_prime(R, M), A is M.

%concat(A1, A2, R) - конкатинирует два массива.
concat([], B, B).
concat([H | T], B, [H | R]) :- concat(T, B, R).

%divisors(N, D, R) - напихает в массив R делители числа N.
divisors(1, _, []).
divisors(N, D, [N]) :- prime(N).
divisors(N, D, R1) :- N >= D * D, 0 is mod(N, D), M is div(N, D), prime(D), divisors(M, D, R2), !, concat([D | _], R2, R1).
divisors(N, D, R1) :- N >= D * D, \+(0 is mod(N, D)), next_prime(D, DN), divisors(N, DN, R1).

%multiply_list([H | T], R) - вернет произведение элементов в массиве.
multiply_list([], R) :- R is 1.
multiply_list([H | T], R) :- multiply_list(T, R1), R is H * R1.

%correct_sort_list([H | T], R)
correct_sort_list([H]) :- prime(H).
correct_sort_list([H1, H2 | T]) :- H1 =< H2, prime(H1), correct_sort_list([H2 | T]).

%prime_divisors - проверяющее, что список Divisors содержит все простые делители числа N, упорядоченные по возрастанию. Если N делится на простое число P несколько раз, то Divisors должен содержать соответствующее число копий P.
prime_divisors(N, R) :- integer(N), divisors(N, 2, R), !.
prime_divisors(N, [H | T]) :- correct_sort_list([H | T]), multiply_list([H | T], N1), N is N1, !.
prime_divisors(N, []) :- N is 1, !.

% EASY
/*
prime(N) - проверяет,что N простое число
inc(N, R) :- number(N), !, R is N + 1.
inc(N, R) :- number(R), !, N is R - 1.
divisible(X, Y) :- X \= Y, N is Y * Y, N =< X, 0 is mod(X, Y).
divisible(X, Y) :- X \= Y, inc(Y, Y1), divisible(X, Y1).
prime(N) :- primeTable(N), !.
prime(N) :- N > 1, Y is 2, \+divisible(N, Y), assert(primeTable(N)).

%composite(N) - проверяет, что N - составное.
composite(N) :- compositeTable(N), !.
composite(N) :- N > 1, \+prime(N), assert(compositeTable(N)).

%next_prime(N, Result) - следующее простое.
next_prime(N, A) :- inc(N, R), prime(R), !, A is R.
next_prime(N, A) :- inc(N, R), \+prime(R), next_prime(R, M), A is M.

%concat(A1, A2, R) - конкатинирует два массива.
concat([], B, B).
concat([H | T], B, [H | R]) :- concat(T, B, R).

%divisors(N, D, R) - напихает в массив R делители числа N.
divisors(1, _, []).
divisors(N, D, R1) :- N >= D, 0 is mod(N, D), M is div(N, D), prime(D), divisors(M, D, R2), !, concat([D | _], R2, R1).
divisors(N, D, R1) :- N >= D, \+(0 is mod(N, D)), next_prime(D, DN), divisors(N, DN, R1).

%multiply_list([H | T], R) - вернет произведение элементов в массиве.
multiply_list([], R) :- R is 1.
multiply_list([H | T], R) :- multiply_list(T, R1), R is H * R1.

%correct_sort_list([H | T], R)
correct_sort_list([H], 1).
correct_sort_list([H1, H2 | T], R) :- H1 =< H2, correct_sort_list([H2 | T], R).
correct_sort_list([H1, H2 | T], R) :- H1 > H2, R is 0.

%prime_divisors - проверяющее, что список Divisors содержит все простые делители числа N, упорядоченные по возрастанию. Если N делится на простое число P несколько раз, то Divisors должен содержать соответствующее число копий P.
prime_divisors(N, R) :- integer(N), divisors(N, 2, R), !.
prime_divisors(N, [H | T]) :- correct_sort_list([H | T], R), 1 is R, multiply_list([H | T], N1), N is N1, !.
prime_divisors(N, []) :- N is 1, !. */