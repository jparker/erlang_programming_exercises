-module(recursion).
-compile(export_all).

bump([])    -> [];
bump([H|T]) -> [H+1 | bump(T)].

avg([])   -> 0;
avg(List) -> sum(List) / len(List).

sum([])    -> 0;
sum([H|T]) -> H + sum(T).

len([])    -> 0;
len([_|T]) -> 1 + len(T).

even([])                      -> [];
even([H|T]) when H rem 2 == 0 -> [H | even(T)];
even([_|T])                   -> even(T).

odd([])                       -> [];
odd([H|T]) when H rem 2 == 1  -> [H | odd(T)];
odd([_|T])                    -> odd(T).

member(_, [])    -> false;
member(E, [E|_]) -> true;
member(E, [_|T]) -> member(E, T).
