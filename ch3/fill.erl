-module(fill).
-export([fill/1, fill/2, open/1]).

-define(DEFAULT_LINE_LENGTH, 72).

fill(File) -> fill(File, ?DEFAULT_LINE_LENGTH).

fill(File, Max) -> fill(open(File), Max, []).

fill(Io, Max, Lines) ->
    case file:read_line(Io) of
        eof  ->
            print(wrap(words(paragraph(Lines)), Max));
        {ok, "\n"} ->
            print(wrap(words(paragraph(Lines)), Max)),
            io:format("~n"),
            fill(Io, Max, []);
        {ok, Line} ->
            fill(Io, Max, [string:strip(Line, right, $\n)|Lines])
    end.

words(Paragraph) -> string:tokens(Paragraph, " ").

paragraph(Lines) -> string:join(lists:reverse(Lines), " ").

wrap(Words, Max) -> wrap(Words, Max, [], 0).

wrap([], _Max, Words, _LineLine) ->
    [string:join(lists:reverse(Words), " ")];
wrap([H|T], Max, [], 0) ->
    wrap(T, Max, [H], length(H));
wrap([H|T], Max, Words, LineLen) when LineLen + 1 + length(H) > Max ->
    [string:join(lists:reverse(Words), " ")|wrap([H|T], Max, [], 0)];
wrap([H|T], Max, Words, LineLen) ->
    wrap(T, Max, [H|Words], LineLen + 1 + length(H)).

print([])    -> ok;
print([H|T]) ->
    io:format("[~3B] ~s~n", [length(H), H]),
    print(T).

open(File) ->
    case file:open(File, [read]) of
        {ok, Io}        -> Io;
        {error, Reason} -> erlang:error({error, Reason})
    end.
