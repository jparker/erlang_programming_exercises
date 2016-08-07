-module(mergesort).
-export([sort/1]).
-include_lib("eunit/include/eunit.hrl").

sort(List) when length(List) > 1 ->
    {A, B} = partition(List),
    merge(sort(A), sort(B));
sort(List) -> List.

merge(List, [])       -> List;
merge([], List)       -> List;
merge([Ha|A], [Hb|B]) ->
    case Ha < Hb of
        true  -> [Ha | merge(A, [Hb|B])];
        false -> [Hb | merge([Ha|A], B)]
    end.

partition(List) -> partitionL(List, [], []).

partitionL([], L, R)    -> {L, R};
partitionL([H|T], L, R) -> partitionR(T, [H|L], R).

partitionR([], L, R)    -> {L, R};
partitionR([H|T], L, R) -> partitionL(T, L, [H|R]).

sort_test() ->
    ?assertEqual([1,2,3,4,5], sort([1,5,2,4,3])),
    ?assertEqual([1,1,1,100], sort([1,100,1,1])).
