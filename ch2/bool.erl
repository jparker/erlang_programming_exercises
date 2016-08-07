-module(bool).
-export([b_not/1, b_and/2, b_or/2, b_nand/2, b_xor/2]).
-include_lib("eunit/include/eunit.hrl").

b_not(false)    -> true;
b_not(_)        -> false.

b_and(false, _) -> false;
b_and(_, false) -> false;
b_and(_, _)     -> true.

b_or(true, _)   -> true;
b_or(_, true)   -> true;
b_or(_, _)      -> false.

b_nand(A, B)    -> b_not(b_and(A, B)).

b_xor(true, false) -> true;
b_xor(false, true) -> true;
b_xor(_, _)        -> false.

b_not_test() ->
    ?assert(b_not(false)),
    ?assertNot(b_not(true)).

b_and_test() ->
    ?assert(b_and(true, true)),
    ?assertNot(b_and(true, false)),
    ?assertNot(b_and(false, true)),
    ?assertNot(b_and(false, false)).

b_or_test() ->
    ?assert(b_or(true, true)),
    ?assert(b_or(true, false)),
    ?assert(b_or(false, true)),
    ?assertNot(b_or(false, false)).

b_nand_test() ->
    ?assert(b_nand(false, false)),
    ?assert(b_nand(false, true)),
    ?assert(b_nand(true, false)),
    ?assertNot(b_nand(true, true)).

b_xor_test() ->
    ?assert(b_xor(true, false)),
    ?assert(b_xor(false, true)),
    ?assertNot(b_xor(true, true)),
    ?assertNot(b_xor(false, false)).
