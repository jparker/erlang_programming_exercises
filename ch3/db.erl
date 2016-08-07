-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).
-include_lib("eunit/include/eunit.hrl").

new()                              -> [].

destroy(_Db)                       -> ok.

write(Key, Value, [])              -> [{Key, Value}];
write(Key, Value, [{Key, _}|Rest]) -> [{Key, Value}|Rest];
write(Key, Value, [Record|Rest])   -> [Record|write(Key, Value, Rest)].

delete(_Key, [])                   -> [];
delete(Key, [{Key, _}|Rest])       -> Rest;
delete(Key, [Record|Rest])         -> [Record|delete(Key, Rest)].

read(_Key, [])                     -> {error, instance};
read(Key, [{Key, Value}|_])        -> {ok, Value};
read(Key, [_|Rest])                -> read(Key, Rest).

match(_Value, [])                  -> [];
match(Value, [{Key, Value}|Rest])  -> [Key|match(Value, Rest)];
match(Value, [_|Rest])             -> match(Value, Rest).

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
