-module(ex1).
-export([sum/1, sum/2]).

sum(N) when N > 0 -> N + sum(N-1);
sum(_)            -> 0.

sum(N, M) when N == M -> N;
sum(N, M) when N < M  -> N + sum(N+1, M).
