-module(text).
-export([print/1, fill/1, fill/2, read/2, sample/0, file/1]).

-define(DEFAULT_LINE_LENGTH, 72).

print([])                     -> ok;
print([Paragraph|Paragraphs]) ->
    print_lines(Paragraph),
    io:format("~n"),
    print(Paragraphs).

print_lines([])               -> ok;
print_lines([Line|Paragraph]) ->
    io:format("[~3B] ~s~n", [length(Line), Line]),
    print_lines(Paragraph).

fill(Io) -> fill(Io, ?DEFAULT_LINE_LENGTH).

fill(Io, Max) -> 
    read(Io, Max).

read(Io, Max) -> read(Io, Max, []).

read(Io, Max, Lines) ->
    case file:read_line(Io) of
        eof        -> [wrap(string:join(lists:reverse(Lines), " "), Max)];
        {ok, "\n"} ->
            Paragraph = wrap(string:join(lists:reverse(Lines), " "), Max),
            [Paragraph|read(Io, Max, [])];
        {ok, Line} ->
            read(Io, Max, [string:strip(Line, right, $\n)|Lines])
    end.

wrap(Paragraph, Max) ->
    wrap(string:tokens(Paragraph, " "), Max, [], 0).

wrap([], _Max, Words, _LineLen) ->
    [join(Words)];
wrap([H|T], Max, Words, LineLen) when LineLen + 1 + length(H) > Max ->
    [join(Words)|wrap([H|T], Max, [], 0)];
wrap([H|T], Max, Words, LineLen) ->
    wrap(T, Max, [H|Words], LineLen + 1 + length(H)).

join(ReversedWords) ->
    string:join(lists:reverse(ReversedWords), " ").

sample() -> file("lorem.txt").

file(File) ->
    case file:open(File, [read]) of
        {ok, Io} -> Io;
        Error    -> erlang:error(Error)
    end.
