-module(ex3).
-export([step/1, even_step/1]).

step(N) -> step(1, N).

step(N, Max) when N > Max -> ok;
step(N, Max) ->
    print(N),
    step(N+1, Max).

even_step(N) -> even_step(1, N).

even_step(N, Max) when N > Max -> ok;
even_step(N, Max) when N rem 2 == 0 ->
    print(N),
    even_step(N+2, Max);
even_step(N, Max) ->
    even_step(N+1, Max).

print(N) ->
    io:format("Number: ~p~n", [N]).
