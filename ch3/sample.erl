-module(sample).
-export([sample/1, sample/2]).

sample(N) -> sample(N, N).

sample(N, Max) when N > 0 ->
    [rand:uniform(Max) | sample(N-1, Max)];
sample(_, _) -> [].
