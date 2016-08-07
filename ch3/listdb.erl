-module(listdb).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).
-include_lib("eunit/include/eunit.hrl").

new()                 -> [].
destroy(_Db)          -> ok.
write(Key, Value, Db) -> lists:keystore(Key, 1, Db, {Key, Value}).
delete(Key, Db)       -> lists:keydelete(Key, 1, Db).

read(Key, Db) ->
    case lists:keyfind(Key, 1, Db) of
        {Key, Value} -> {ok, Value};
        false        -> {error, instance}
    end.

match(Value, Db) ->
    lists:filtermap(
      fun({K,V}) ->
              case Value == V of
                    true  -> {true, K};
                    false -> false
                end
      end, Db).

new_test() ->
    ?assertEqual([], new()).

destroy_test() ->
    Db = new(),
    ?assertEqual(ok, destroy(Db)).

write_test() ->
    Db = write(one, "one", new()),
    ?assertEqual([{one, "one"}, {two, "two"}], write(two, "two", Db)),
    ?assertEqual([{one, "uno"}], write(one, "uno", Db)).

delete_test() ->
    Db = write(two, "two", write(one, "one", new())),
    ?assertEqual([{two, "two"}], delete(one, Db)),
    ?assertEqual([{one, "one"}, {two, "two"}], delete(three, Db)).

read_test() ->
    Db = write(one, "one", new()),
    ?assertEqual({ok, "one"}, read(one, Db)),
    ?assertEqual({error, instance}, read(two, Db)).

match_test() ->
    Db = write(uno, "one", write(two, "two", write(one, "one", new()))),
    ?assertEqual([one, uno], match("one", Db)),
    ?assertEqual([two], match("two", Db)),
    ?assertEqual([], match("three", Db)).
