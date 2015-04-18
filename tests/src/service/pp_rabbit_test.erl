%% -*- mode: nitrogen -*-
-module(pp_rabbit_test).
-include_lib("eunit/include/eunit.hrl").

main_test() ->
    Add1 = fun(A, B) -> A + B end,
    Add2 = fun(A, B) -> A + B + 1 end,
    Queues = [add1, add2],
    pp_rabbit:start(),

    % ?assertMatch(<<"3">>, pp_rabbit:call(add, [1,2])),

    pp_rabbit:stop(),
    ok.
