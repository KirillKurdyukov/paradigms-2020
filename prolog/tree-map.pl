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
map_get(TreeMap, Key, Value) :-
	split(TreeMap, Key, (L, R)),
	NKey is 1 + Key,
	split(R, NKey, (NL, NR)),
	NL \= null,
	split(R, NKey, (node(Key1,Value1, Y, null, null), NR)),
	Value1 is Value, !.

map_Key(TreeMap, Key, Value) :-
	split(TreeMap, Key, (L, R)),
	NKey is 1 + Key,
	split(R, NKey, (NL, NR)),
	NL \= null.

map_put(TreeMap, Key, Value, Result) :-
	map_Key(TreeMap, Key, Value1),
	split(TreeMap, Key, (L, R)),
	split(R, Key + 1, (NL, NR)),
	rand_int(1000000, Y),
	merge(L, node(Key, Value, Y, null, null), Left),
	merge(Left, NR, Result).

map_remove(TreeMap, Key, Result)	:-
	split(TreeMap, Key, (L, R)),
	split(R, Key + 1, (NL, NR)),
	merge(L, NR, Result).

map_build([(Key, Val)], TreeMap) :-
	map_put(null, Key, Value, TreeMap), !.
map_build([(Key, Val) | T], TreeMap) :-
	map_build(T, NTreeMap),
	map_put(NTreeMap, Key, Value, TreeMap), !.