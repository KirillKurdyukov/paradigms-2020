node(Key, Val, Y, L, R).
%merge - OK
merge(null, T, T) :- !.
merge(T, null, T) :- !.
merge(node(Key1, Val1, Y1, L1, R1), node(Key2, Val2, Y2, L2, R2), node(Key1, Val1, Y1, L1, Right)) :-
	Y2 < Y1,
	merge(R1, node(Key2, Val2, Y2, L2, R2), Right), !.
merge(node(Key1, Val1, Y1, L1, R1), node(Key2, Val2, Y2, L2, R2), node(Key2, Val2, Y2, Left, R2)) :-
	Y2 >= Y1,
	merge(node(Key1, Val1, Y1, L1, R1), L2, Left), !.

%split - OK
split(null, Key, (null, null)) :- !.

split(node(Key, Val, Y, Left, Rigth), X, (node(Key, Val, Y, Left, NRigth), NR)) :-
	Key < X,
	split(Rigth, X, (NRigth, NR)),
	!.
split(node(Key, Val, Y, Left, Rigth), X, (NL, node(Key, Val, Y, NLeft, Rigth))) :-
	Key >= X,
	split(Left, X, (NL, NLeft)),
	!.
%map_get - OK
map_get(TreeMap, Key, Val) :-
	split(TreeMap, Key, (L, R)),
	NKey is Key + 1,
	split(R, NKey, (node(Key, Val, Y, _, _), _)), !.

%map_remove - OK
map_remove(TreeMap, Key, Result)	:-
	split(TreeMap, Key, (L, R)),
	split(R, Key + 1, (NL, NR)),
	merge(L, NR, Result), !.

%map_put - OK
map_put(TreeMap, Key, Value, Result) :-
	split(TreeMap, Key, (L1, R1)),
	NKey is Key + 1,
	split(R1, NKey, (L2, R2)),
	rand_int(1000000, Y),
	merge(L1, node(Key, Value, Y, null, null), Left),
	merge(Left, R2, Result), !.

%map_build - OK
map_build([], null) :- !.
map_build([(Key, Val) | T], TreeMap) :-
	map_build(T, NTreeMap),
	map_put(NTreeMap, Key, Val, TreeMap), !.
% ADDITION
map_Key(TreeMap, Key, _) :-
	split(TreeMap, Key, (L, R)),
	NKey is Key + 1,
	split(R, NKey, (node(Key, _, Y, _, _), _)), !.

map_replace(null, _, _, null) :- !.
map_replace(Map, Key, Value, Result) :-
    map_Key(Map, Key, Value),
	map_put(Map, Key, Value, Result), !.
map_replace(Map, Key, Value, Map) :-
    \+map_Key(Map, Key, Value), !.
