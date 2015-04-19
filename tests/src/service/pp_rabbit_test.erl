%% -*- mode: nitrogen -*-
-module(pp_rabbit_test).
-export([myadd1/2, myadd2/2]).
-include_lib("eunit/include/eunit.hrl").

manual_rpc() ->
    Add1 = fun(A, B) -> A + B end,

    %% common rpc client/server by erlang
    pp_rabbit:reg_rpc_server(add1, Add1),
    pp_rabbit:reg_rpc_client(add1),
    ?assertMatch(<<"3">>, pp_rabbit:rpc_call(add1, [1,2])),

    %% rpc call by json
    ?assertMatch(<<"3">>, pp_rabbit:rpc_call(add1, <<"[1,2]">>)),

    %% exception in rpc client call
    ?assertMatch(<<"error">>, pp_rabbit:rpc_call(add1, <<"\"[1,2]\"">>)),
    %% invalid rpc param call
    ?assertMatch(<<"[error,", _Rest/binary>>, pp_rabbit:rpc_call(add1, {1, 2, 1})),

    %% rpc pipeline queue
    Add2 = fun(A, B) -> A + B + 1 end,
    pp_rabbit:reg_rpc_server(add1, Add2),

    ?assertMatch(<<"3">>, pp_rabbit:rpc_call(add1, [1,2])),
    ?assertMatch(<<"4">>, pp_rabbit:rpc_call(add1, [1,2])),

    pp_rabbit:unreg_rpc_client(add1),
    ?assertMatch(undefined, pp_rabbit:rpc_call(add1, [1,2])),
    pp_rabbit:reg_rpc_client(add1),

    %% utf8
    Add3 = fun(A, B) -> <<"结果是: "/utf8, (pp:to_binary(A + B))/binary>> end,
    pp_rabbit:reg_rpc_server(add3, Add3),
    pp_rabbit:reg_rpc_client(add3),
    R = pp_rabbit:rpc_call(add3, [1,2]),
    ?assertMatch(<<"结果是: 3"/utf8>>, jiffy:decode(R)),

    ok.

myadd1(A, B) -> A + B.
myadd2(A, B) -> A + B + 1.

-define(TabServer, test_rabbit_rpc_server).
-define(TabClient, test_rabbit_rpc_client).
main_test() ->
    %% init rpc servers and clients
    nosqlite:create_table(?TabServer, ram),
    nosqlite:create_table(?TabClient, ram),
    Servers = [
        {add5, ?MODULE, myadd1, 2},
        {add6, ?MODULE, myadd2, 2}
    ],
    pp_rabbit:reg_auto_server(?TabServer, Servers),

    Clients = [add5, add6],
    pp_rabbit:reg_auto_client(?TabClient, Clients),

    %% start
    pp_rabbit:start(?TabClient, ?TabServer),

    %% test rpc call
    ?assertMatch(<<"3">>, pp_rabbit:rpc_call(add5, [1,2])),
    ?assertMatch(<<"4">>, pp_rabbit:rpc_call(add6, [1,2])),

    %% manul rpc server and client
    manual_rpc(),

    %% stop
    pp_rabbit:stop(),
    ok.
