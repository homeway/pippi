%% -*- mode: nitrogen -*-
-module(pp_rabbit_test).
-include_lib("eunit/include/eunit.hrl").

main_test() ->
    Add1 = fun(A, B) -> A + B end,
    pp_rabbit:start(),

    %% common rpc client/server by erlang
    pp_rabbit:reg_rpc_server(add1, Add1),
    pp_rabbit:reg_rpc_client(add1),
    ?assertMatch(<<"3">>, pp_rabbit:rpc_call(add1, [1,2])),

    %% rpc call by json
    ?assertMatch(<<"3">>, pp_rabbit:rpc_call(add1, <<"[1,2]">>)),

    %% exception in rpc client call
    ?assertMatch(<<"error">>, pp_rabbit:rpc_call(add1, <<"\"[1,2]\"">>)),
    %% invalid rpc param call
    ?assertMatch(<<"[error,", Rest/binary>>, pp_rabbit:rpc_call(add1, {1, 2, 1})),

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

    pp_rabbit:stop(),
    ok.
