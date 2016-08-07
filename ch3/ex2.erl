-module(ex2).
-export([create/1, reverse_create/1]).

create(N) -> create(N, []).

create(N, List) when N > 0 -> create(N-1, [N|List]);
create(_, List) -> List.

reverse_create(0) -> [];
reverse_create(N) -> [N|reverse_create(N-1)].
