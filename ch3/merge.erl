-module(merge).
-compile(export_all).

merge(Xs, Ys) ->
    lists:reverse(mergeL(Xs, Ys, [])).

mergeL([], [], Zs)     -> Zs;
mergeL([], [Y|Ys], Zs) -> mergeR([], [Y|Ys], Zs);
mergeL([X|Xs], Ys, Zs) -> mergeR(Xs, Ys, [X|Zs]).

mergeR([], [], Zs)     -> Zs;
mergeR([X|Xs], [], Zs) -> mergeL([X|Xs], [], Zs);
mergeR(Xs, [Y|Ys], Zs) -> mergeL(Xs, Ys, [Y|Zs]).
