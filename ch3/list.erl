-module(list).
-export([filter/2, reverse/1, concat/1, flatten/1]).
-include_lib("eunit/include/eunit.hrl").

filter([], _Max)                 -> [];
filter([H|T], Max) when H =< Max -> [H|filter(T, Max)];
filter([_|T], Max)               -> filter(T, Max).

reverse(List)                    -> reverse(List, []).

reverse([], List)                -> List;
reverse([H|T], List)             -> reverse(T, [H|List]).

concat(List)                     -> concat(List, []).

concat([], List)                 -> reverse(List);
concat([H|T], List)              -> concat(T, add(H, List)).

add([], List)                    -> List;
add([H|T], List)                 -> add(T, [H|List]);
add(E, List)                     -> [E|List].

flatten([])                      -> [];
flatten([H|T])                   -> concat([flatten(H), flatten(T)]);
flatten(Element)                 -> Element.

filter_test() ->
    ?assertEqual([1,2,3], filter([1,2,3,4,5], 3)),
    ?assertEqual([1,2], filter([1,2,3,4,5], 2)).

reverse_test() ->
    ?assertEqual([3,2,1], reverse([1,2,3])).

concat_test() ->
    ?assertEqual([1,2,3,4,five], concat([[1,2,3],[],[4,five]])).

flatten_test() ->
    ?assertEqual([1,2,3,4,5,6], flatten([[1,[2,[3],[]]],[[[4]]],[5,6]])).
