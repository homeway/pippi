%% -*- mode: nitrogen -*-
-module(pp_rabbit_lib_test).
-include_lib("eunit/include/eunit.hrl").

main_test() ->
    Conn = pp_rabbit_lib:connect(),
    Ch = Conn:channel(),
    %% clear exchanges and queues to test
    Exchanges = [ex1, ex2, ex3],
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

    %% prepare func to fanout/direct/topic
    Pid = self(),
    Add3 = fun(A, B) -> Pid ! (A+B) end,
    Add4 = fun(A, B) -> Pid ! (A+B) end,

    %% service call - fanout
    S1 = Ch:service_reg(ex1, <<"">>, fanout, Add3),
    receive none -> wait after 500 -> ok end,
    Ch:basic_publish(ex1, <<"">>, [1,2]),
    ?assertMatch(3, got_msg(1000)),

    S3 = Ch:service_reg(ex1, <<"">>, fanout, Add4),
    Ch:basic_publish(ex1, <<"">>, [50,50]),
    ?assertMatch(100, got_msg(1000)),
    ?assertMatch(100, got_msg(1000)),

    %% service call - direct
    S4 = Ch:service_reg(ex2, mydirect, direct, Add3),
    receive none -> wait after 500 -> ok end,

    Ch:basic_publish(ex2, mydirect, [1,1]),
    ?assertMatch(2, got_msg(1000)),

    %% service call - topic
    S5 = Ch:service_reg(ex3, <<"mytopic.*">>, topic, Add3),
    S6 = Ch:service_reg(ex3, <<"*.inc">>, topic, Add4),
    receive none -> wait after 500 -> ok end,

    Ch:basic_publish(ex3, <<"mytopic.add">>, [1,1]),
    ?assertMatch(2, got_msg(1000)),

    Ch:basic_publish(ex3, <<"mytopic2.inc">>, [1,1]),
    ?assertMatch(2, got_msg(1000)),

    Ch:basic_publish(ex3, <<"mytopic.inc">>, [2,2]),
    ?assertMatch(4, got_msg(1000)),
    ?assertMatch(4, got_msg(1000)),

    [S:service_unreg() || S <- [S1, S3, S4, S5, S6]],

    flush(),
    ok.

got_msg(Timeout) ->
    receive  Msg ->
        Msg
    after    Timeout ->
        {text, <<"\"nothing\"">>} end.

flush() ->
    receive Msg ->
        erlang:display(Msg),
        flush()
    after 100 ->
        ok end.
