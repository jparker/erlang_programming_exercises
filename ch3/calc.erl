-module(calc).
-export([parse/1, eval/1, stringify/1, compile/1, simulate/1, optimize/1]).
-include_lib("eunit/include/eunit.hrl").

-define(PRECEDENCES, #{
          uminus   => 3,
          multiply => 2,
          divide   => 2,
          add      => 1,
          subtract => 1,
          lparen   => 0,
          rparen   => 0
         }).
-define(PRECEDENCE(Op), maps:get(Op, ?PRECEDENCES)).

parse("")     -> {};
parse(Buffer) -> parse(tokenize(Buffer), [], []).

% This is an implementation of the Shunting-yard algorithm.
% <URL:https://en.wikipedia.org/wiki/Shunting-yard_algorithm>
parse([], [], [Exp]) -> Exp;
parse([], [lparen|Ops], Exp) ->
    erlang:error({error, mismatched_parentheses, [], [lparen|Ops], Exp});
parse([], [Op|Ops], Exp) ->
    parse([], Ops, assemble(Op, Exp));
parse([Num|Tokens], Ops, Exp) when is_integer(Num) ->
    parse(Tokens, Ops, [{num, Num}|Exp]);
parse([lparen|Tokens], Ops, Exp) ->
    parse(Tokens, [lparen|Ops], Exp);
parse([rparen|Tokens], [lparen|Ops], Exp) ->
    parse(Tokens, Ops, Exp);
parse([rparen|Tokens], [Op1|Ops], Exp) ->
    parse([rparen|Tokens], Ops, assemble(Op1, Exp));
parse([rparen|Tokens], [], Exp) ->
    erlang:error({error, mismatched_parentheses, [rparen|Tokens], [], Exp});
parse([Op1|Tokens], [Op2|Ops], Exp) ->
    case ?PRECEDENCE(Op1) =< ?PRECEDENCE(Op2) of
        true  -> parse([Op1|Tokens], Ops, assemble(Op2, Exp));
        false -> parse(Tokens, [Op1,Op2|Ops], Exp)
    end;
parse([Op1|Tokens], [], Exp) ->
    parse(Tokens, [Op1], Exp).

assemble(uminus, [A|Exp]) -> [{uminus, A}|Exp];
assemble(Op,   [B,A|Exp]) -> [{Op, A, B}|Exp].

tokenize([])          -> [];
tokenize([32 | Rest]) -> tokenize(Rest);
tokenize([$( | Rest]) -> [lparen   | tokenize(Rest)];
tokenize([$) | Rest]) -> [rparen   | tokenize(Rest)];
tokenize([$+ | Rest]) -> [add      | tokenize(Rest)];
tokenize([$- | Rest]) -> [subtract | tokenize(Rest)];
tokenize([$* | Rest]) -> [multiply | tokenize(Rest)];
tokenize([$/ | Rest]) -> [divide   | tokenize(Rest)];
tokenize([$~ | Rest]) -> [uminus   | tokenize(Rest)];
tokenize(Buffer) ->
    case string:to_integer(Buffer) of
        { error, Reason } -> erlang:error({error, Reason, Buffer});
        { Num,   Rest   } -> [Num | tokenize(Rest)]
    end.

eval({num, Num})       -> Num;
eval({uminus,   A})    -> -eval(A);
eval({add,      A, B}) -> eval(A) + eval(B);
eval({subtract, A, B}) -> eval(A) - eval(B);
eval({multiply, A, B}) -> eval(A) * eval(B);
eval({divide,   A, B}) -> eval(A) / eval(B).

stringify({num, Num})       -> integer_to_list(Num);
stringify({uminus,   A})    -> "~" ++ stringify(A);
stringify({add,      A, B}) -> "(" ++ stringify(A) ++ "+" ++ stringify(B) ++ ")";
stringify({subtract, A, B}) -> "(" ++ stringify(A) ++ "-" ++ stringify(B) ++ ")";
stringify({multiply, A, B}) -> "(" ++ stringify(A) ++ "*" ++ stringify(B) ++ ")";
stringify({divide,   A, B}) -> "(" ++ stringify(A) ++ "/" ++ stringify(B) ++ ")".

compile(Exp) -> compile(Exp, []).

compile({num, Num}, Stack) -> [{push, Num} | Stack];
compile({Op, A},    Stack) -> compile(A, [Op | Stack]);
compile({Op, A, B}, Stack) -> compile(A, compile(B, [Op | Stack])).

simulate(Code) -> simulate(Code, []).

simulate([], [Top])                 -> Top;
simulate([{push, Num}|Code], Stack) -> simulate(Code, [Num|Stack]);
simulate([uminus|Code], [A|Stack])  -> simulate(Code, [-A|Stack]);
simulate([Op|Code], [B,A|Stack])    ->
    case Op of
        add      -> simulate(Code, [A+B|Stack]);
        subtract -> simulate(Code, [A-B|Stack]);
        multiply -> simulate(Code, [A*B|Stack]);
        divide   -> simulate(Code, [A/B|Stack])
    end.

optimize({num, Num})                 -> {num, Num};
optimize({uminus, {num, 0}})         -> {num, 0};
optimize({uminus, {uminus, A}})      -> optimize(A);
optimize({uminus, A})                -> {uminus, optimize(A)};
optimize({add, A, {num, 0}})         -> optimize(A);
optimize({add, {num, 0}, B})         -> optimize(B);
optimize({add, A, {uminus, B}})      -> {subtract, optimize(A), optimize(B)};
optimize({add, {uminus, A}, B})      -> {subtract, optimize(B), optimize(A)};
optimize({subtract, A, {num, 0}})    -> optimize(A);
optimize({subtract, A, A})           -> {num, 0};
optimize({subtract, A, {uminus, B}}) -> {add, optimize(A), optimize(B)};
optimize({multiply, _, {num, 0}})    -> {num, 0};
optimize({multiply, {num, 0}, _})    -> {num, 0};
optimize({multiply, A, {num, 1}})    -> optimize(A);
optimize({multiply, {num, 1}, B})    -> optimize(B);
optimize({divide, {num, 0}, _})      -> {num, 0};
optimize({divide, A, {num, 1}})      -> optimize(A);
optimize({divide, A, A})             -> {num, 1};
optimize({Op, A, B})                 ->
    {OA, OB} = {optimize(A), optimize(B)},
    case {OA, OB} of
        {A, B} -> {Op, A, B};
        _      -> optimize({Op, OA, OB})
    end.

parse_test() ->
    ?assertEqual({num,42}, parse("42")),
    ?assertEqual({add,{num,1},{num,2}}, parse("1+2")),
    ?assertEqual(
       {subtract,{add,{num,2},{num,3}},{num,4}},
       parse("((2+3)-4)")
      ),
    ?assertEqual(
       {uminus,
        {add,
         {multiply,{num,2},{num,3}},
         {multiply,{num,3},{num,4}}
        }
       },
       parse("~(2*3+3*4)")
      ),
    ?assertEqual(
       {add,
        {multiply,{uminus,{num,2}},{num,3}},
        {multiply,{num,3},{num,4}}
       },
       parse("~2*3+3*4")
      ),
    ?assertEqual(
       {divide,
        {multiply,
         {uminus,{subtract,{add,{num,1},{num,2}},{num,3}}},
         {num,4}},
        {num,5}},
       parse("(~((1+2-3))*4/5)")
      ).

eval_test() ->
    ?assertEqual(42, eval(parse("42"))),
    ?assertEqual(3, eval(parse("1+2"))),
    ?assertEqual(1, eval(parse("((2+3)-4)"))),
    ?assertEqual(-18, eval(parse("~(2*3+3*4)"))),
    ?assertEqual(6, eval(parse("~2*3+3*4"))),
    ?assertEqual(-1.4, eval(parse("~1+2-3*4/5"))),
    ?assertEqual(0.0, eval(parse("~((1+2-3)*4/5)"))).

stringify_test() ->
    ?assertEqual("42", stringify(parse("42"))),
    ?assertEqual("(1+2)", stringify(parse("1+2"))),
    ?assertEqual("((2+3)-4)", stringify(parse("((2+3)-4)"))),
    ?assertEqual("~((2*3)+(3*4))", stringify(parse("~(2*3+3*4)"))),
    ?assertEqual("((~2*3)+(3*4))", stringify(parse("~2*3+3*4"))).

compile_test() ->
    ?assertEqual([{push,42}], compile(parse("42"))),
    ?assertEqual([{push,1},{push,2},add], compile(parse("1+2"))),
    ?assertEqual(
       [{push,2},{push,3},add,{push,4},subtract],
       compile(parse("((2+3)-4)"))
      ),
    ?assertEqual(
       [{push,2},{push,3},multiply,{push,3},{push,4},multiply,add,uminus],
       compile(parse("~(2*3+3*4)"))
      ),
    ?assertEqual(
       [{push,1},{push,2},add,{push,3},subtract,uminus,{push,4},multiply,{push,5},divide],
       compile(parse("~((1+2-3))*4/5"))
      ).

simulate_test() ->
    ?assertEqual(42, simulate(compile(parse("42")))),
    ?assertEqual(3, simulate(compile(parse("1+2")))),
    ?assertEqual(1, simulate(compile(parse("((2+3)-4)")))),
    ?assertEqual(-18, simulate(compile(parse("~(2*3+3*4)")))),
    ?assertEqual(6, simulate(compile(parse("~2*3+3*4")))),
    ?assertEqual(-1.4, simulate(compile(parse("~1+2-3*4/5")))),
    ?assertEqual(0.0, simulate(compile(parse("~((1+2-3)*4/5)")))).

optimize_test() ->
    ?assertEqual({num,1}, optimize(parse("1+0"))),
    ?assertEqual({num,1}, optimize(parse("1+0"))),
    ?assertEqual({num,1}, optimize(parse("1+0"))),
    ?assertEqual({num,1}, optimize(parse("0+1"))),
    ?assertEqual({num,1}, optimize(parse("1-0"))),
    ?assertEqual({num,0}, optimize(parse("2-2"))),
    ?assertEqual({num,0}, optimize(parse("1*0"))),
    ?assertEqual({num,0}, optimize(parse("0*1"))),
    ?assertEqual({num,2}, optimize(parse("2*1"))),
    ?assertEqual({num,2}, optimize(parse("1*2"))),
    ?assertEqual({num,0}, optimize(parse("0/1"))),
    ?assertEqual({num,2}, optimize(parse("2/1"))),
    ?assertEqual({num,1}, optimize(parse("~(~(~(~1)))"))),
    ?assertEqual({num,0}, optimize(parse("~0"))),
    ?assertEqual({subtract,{num,1},{num,2}}, optimize(parse("~2+1"))),
    ?assertEqual({subtract,{num,2},{num,1}}, optimize(parse("2+~1"))),
    ?assertEqual({num,0}, optimize(parse("(3-3)*4+1-(2/2)"))),
    ?assertEqual(
       {divide,{add,{num,1},{num,2}},{multiply,{num,3},{num,4}}},
       optimize(parse("(1+0+2)/(3*4/1)"))
      ).
