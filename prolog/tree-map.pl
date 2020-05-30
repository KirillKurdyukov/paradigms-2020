node(Key, Val, Y, L, R).
merge(null, T, T) :- !.
merge(T, null, T) :- !.
merge(node(Key1, Val1, Y1, L1, R1), node(Key2, Val2, Y2, L2, R2), node(Key1, Val1, Y1, L1, NR1)) :-
	Y1 > Y2, 
	merge(R1, node(Key2, Val2, Y2, R2, L2), NR1),
	!.
merge(node(Key1, Val1, Y1, L1, R1), node(Key2, Val2, Y2, L2, R2), node(Key2, Val2, Y2, NL2, R2)) :-
	Y1 =< Y2, 
	merge(node(Key1, Val1, Y1, L1, R1), L2, NL2), 
	!.
split(null, Key, (null, null)) :- !.

split(node(Key, Val, Y, Left, Rigth), X, (node(Key, Val, Y, Left, NRigth), NR)) :-
	Key < X, 
	split(Rigth, X, (NRigth, NR)),
	!. 
split(node(Key, Val, Y, Left, Rigth), X, (NL, node(Key, Val, Y, NLeft, Rigth))) :-
	Key >= X, 
	split(Left, X, (NL, NLeft)),
	!. 
