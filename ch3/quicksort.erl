-module(quicksort).
-export([sort/1]).
-include_lib("eunit/include/eunit.hrl").

sort([])    -> [];
sort([H|T]) ->
    {A, B} = partition(H, T, [], []),
    sort(A) ++ [H] ++ sort(B).

partition(_, [], A, B)    -> {A, B};
partition(P, [H|T], A, B) ->
    case H < P of
        true  -> partition(P, T, [H|A], B);
        false -> partition(P, T, A, [H|B])
    end.

sort_test() ->
    ?assertEqual([1,2,3,4,5], sort([1,5,2,4,3])),
    ?assertEqual([1,1,1,100], sort([1,100,1,1])).
