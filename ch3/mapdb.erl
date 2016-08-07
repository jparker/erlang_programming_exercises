-module(mapdb).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).
-include_lib("eunit/include/eunit.hrl").

new()                 -> #{}.
destroy(_Db)          -> ok.
write(Key, Value, Db) -> maps:put(Key, Value, Db).
delete(Key, Db)       -> maps:remove(Key, Db).

read(Key, Db) ->
    case maps:find(Key, Db) of
        {ok, Value} -> {ok, Value};
        error       -> {error, instance}
    end.

match(Value, Db) ->
    maps:keys(maps:filter(fun(_,V) -> Value == V end, Db)).

new_test() ->
    ?assertEqual(#{}, new()).

destroy_test() ->
    ?assertEqual(ok, destroy(new())).

write_test() ->
    Db = write(one, "one", new()),
    ?assertEqual(#{one => "one", two => "two"}, write(two, "two", Db)),
    ?assertEqual(#{one => "uno"}, write(one, "uno", Db)).

delete_test() ->
    Db = write(one, "one", new()),
    ?assertEqual(#{}, delete(one, Db)),
    ?assertEqual(#{one => "one"}, delete(two, Db)).

read_test() ->
    Db = write(one, "one", new()),
    ?assertEqual({ok, "one"}, read(one, Db)),
    ?assertEqual({error, instance}, read(two, Db)).

match_test() ->
    Db = write(uno, "one", write(two, "two", write(one, "one", new()))),
    ?assertEqual([one, uno], match("one", Db)),
    ?assertEqual([two], match("two", Db)),
    ?assertEqual([], match("three", Db)).
