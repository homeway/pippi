%% -*- mode: nitrogen -*-
-module(pp_rabbit_lib_test).
-include_lib("eunit/include/eunit.hrl").

main_test() ->
    Conn = pp_rabbit_lib:connect(),
    Ch = Conn:channel(),
    %% clear exchanges and queues to test
    Exchanges = [],
    Queues = [test_add],
    [ Ch:exchange_delete(E) || E <- Exchanges ],
    [ Ch:queue_delete(Q) || Q <- Queues],

    %% prepare function to reg
    Add1 = fun(A, B) -> A + B end,
    Add2 = fun(A, B) -> A + B + 1 end,

    %% rpc call
    Ch:rpc_reg(test_add, Add1),
    ?assertMatch({_, <<"3">>}, Ch:rpc_call(test_add, [1,2])),

    %% rpc call queue
    Ch:rpc_reg(test_add, Add2),
    ?assertMatch({_, <<"3">>}, Ch:rpc_call(test_add, [1,2])),
    ?assertMatch({_, <<"4">>}, Ch:rpc_call(test_add, [1,2])),
    ?assertMatch({_, <<"3">>}, Ch:rpc_call(test_add, [1,2])),
    ?assertMatch({_, <<"4">>}, Ch:rpc_call(test_add, [1,2])),
    ?assertMatch({_, <<"3">>}, Ch:rpc_call(test_add, [1,2])),
    ?assertMatch({_, <<"4">>}, Ch:rpc_call(test_add, [1,2])),

    %% service call
    ok.
