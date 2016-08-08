-module(index).
-export([raw_document/1, document/1, index/1, print/1]).

-define(SEPARATORS, " \t\n,:;.!?").

index(File) -> index(document(File), 1, #{}).

index([],   _LineNo, Index) ->
    maps:map(fun(_, LineNos) -> uniq(LineNos, -1, []) end, Index);
index([H|T], LineNo, Index) ->
    index(T, LineNo+1, index_line(H, LineNo, Index)).

uniq([],    _, List) -> List;
uniq([H|T], H, List) -> uniq(T, H, List);
uniq([H|T], _, List) -> uniq(T, H, [H|List]).

index_line([],   _LineNo, Index) -> Index;
index_line([H|T], LineNo, Index) ->
    index_line(T, LineNo, update_index(H, LineNo, Index)).

update_index(Word, LineNo, Index) ->
    maps:update_with(
      Word,
      fun(LineNos) -> [LineNo|LineNos] end,
      [LineNo],
      Index
     ).

document(File) -> scan(raw_document(File)).

scan([]) -> [];
scan([Line|Lines]) ->
    [string:tokens(Line, ?SEPARATORS)|scan(Lines)].

raw_document(File) ->
    case file:open(File, [read]) of
        {ok, Io} -> read(Io);
        Error    -> erlang:error(Error)
    end.

read(Io) ->
    case file:read_line(Io) of
        {ok, Line} -> [Line|read(Io)];
        eof        -> []
    end.

print([]) -> ok;
print([{Word, LineNos}|Index]) ->
    io:format("~-20s ~s~n", [Word, ranges(LineNos)]),
    print(Index);
print(Map) when is_map(Map) ->
    print(lists:keysort(1, maps:to_list(Map))).

ranges(LineNos) ->
    string:join(stringify(LineNos), ",").

stringify(LineNos) ->
    lists:map(
      fun(Range) -> join(Range) end,
      lines_to_tuples(LineNos, -1, -1)
     ).

join({A, A}) -> integer_to_list(A);
join({A, B}) -> integer_to_list(A) ++ "-" ++ integer_to_list(B).

lines_to_tuples([], F, L)      -> [{F,L}];
lines_to_tuples([H|T], -1, -1) -> lines_to_tuples(T, H, H);
lines_to_tuples([H|T], F, L)   ->
    case H - L of
        1 -> lines_to_tuples(T, F, H);
        _ -> [{F,L}|lines_to_tuples(T, H, H)]
    end.
