-module(tail_recursion).
-compile(export_all).

bump(List) -> bump(List, []).

bump([], Acc) -> reverse(Acc);
bump([H|T], Acc) -> bump(T, [H+1 | Acc]).

avg([])   -> 0;
avg(List) -> avg(List, 0, 0).

avg([], Sum, Len)    -> Sum / Len;
avg([H|T], Sum, Len) -> avg(T, Sum+H, Len+1).

sum(List) -> sum(List, 0).

sum([], Acc)    -> Acc;
sum([H|T], Acc) -> sum(T, Acc + H).

len(List) -> len(List, 0).

len([], Acc)    -> Acc;
len([_|T], Acc) -> len(T, Acc + 1).

even(List) -> even(List, []).

even([], Acc)                      -> reverse(Acc);
even([H|T], Acc) when H rem 2 == 0 -> even(T, [H|Acc]);
even([_|T], Acc)                   -> even(T, Acc).

odd(List) -> odd(List, []).

odd([], Acc)                      -> reverse(Acc);
odd([H|T], Acc) when H rem 2 /= 0 -> odd(T, [H|Acc]);
odd([_|T], Acc)                   -> odd(T, Acc).

reverse(List) -> reverse(List, []).

reverse([], Acc)    -> Acc;
reverse([H|T], Acc) -> reverse(T, [H|Acc]).
