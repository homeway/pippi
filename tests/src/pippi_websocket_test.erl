%% -*- mode: nitrogen -*-
-module(pippi_websocket_test).
-export([myadd1/2, myadd2/2]).
-include_lib("eunit/include/eunit.hrl").

-define(TabServer, test_rabbit_rpc_server).
-define(TabClient, test_rabbit_rpc_client).
myadd1(A, B) -> A + B.
myadd2(A, B) -> A + B + 1.

clear_msg() ->
    receive  _Msg -> clear_msg()
    after      50 -> ok end.

got_msg() ->
    receive  Msg -> Msg
    after    50 -> {text, <<"\"nothing\"">>} end.

ws_req(Cmd) ->
    jiffy:encode(Cmd).

ws_resp() ->
    {text, R} = got_msg(),
    {text, jiffy:decode(R, [return_maps])}.

main_test() ->
    pp_account_test:prepare(),
    nosqlite:create_table(users, ram),
    nosqlite:clear_table(users),

    Ws = pp_websocket_client:connect("ws://127.0.0.1:8964/ws"),

    %% call status
    clear_msg(),
    Ws:send_text(ws_req([status])),
    ?assertMatch({text, [<<"offline">>, #{}]}, ws_resp()),

    %% call methods
    clear_msg(),
    Ws:send_text(ws_req([methods])),
    ?assertMatch({text, [_|_]}, ws_resp()),

    %% call {nosqlite, users}:size() when offline
    clear_msg(),
    Ws:send_text(ws_req([[<<"nosqlite">>, <<"users">>], <<"size">>])),
    ?assertMatch({text, 0}, ws_resp()),
    %% call deny method
    Ws:send_text(ws_req([<<"users">>, <<"total">>])),
    ?assertMatch({text, [<<"error">>, <<"deny">>|_]}, ws_resp()),

    %% login
    clear_msg(),
    Ws:send_text(ws_req([<<"login">>, ["adi", "123"]])),
    ?assertEqual({text, <<"ok">>}, ws_resp()),

    %% online call
    %%

    %% call status
    clear_msg(),
    Ws:send_text(ws_req([<<"status">>])),
    Status1 = ws_resp(),
    pp:display(Status1),
    ?assertMatch({text, [<<"online">>, #{}]}, Status1),

    %% call users:size()
    Ws:send_text(ws_req([<<"users">>, <<"size">>])),
    ?assertMatch({text, 10}, ws_resp()),

    %% call {nosqlite, users}:size()
    clear_msg(),
    Ws:send_text(ws_req([[<<"nosqlite">>, <<"users">>], <<"size">>])),
    ?assertMatch({text, 0}, ws_resp()),

    %% call {nosqlite, users}:create(1, #{name=>adi})
    Ws:send_text(ws_req([[<<"nosqlite">>, <<"users">>], <<"create">>, [<<"1">>, #{name=>adi}]])),
    ?assertMatch({text, <<"ok">>}, ws_resp()),

    %% rpc over pp_rabbit/amqp
    %%

    %% config pp_rabbit
    nosqlite:create_table(?TabServer, ram),
    nosqlite:create_table(?TabClient, ram),
    Servers = [
        {'test.add5', ?MODULE, myadd1, 2},
        {'test.add6', ?MODULE, myadd2, 2}
    ],
    pp_rabbit:reg_rpc_tab_server(?TabServer, Servers),

    Clients = ['test.add5', 'test.add6'],
    pp_rabbit:reg_rpc_tab_client(?TabClient, Clients),

    %% start pp_rabbit
    pp_rabbit:start(?TabClient, ?TabServer),

    %% test rpc call with user adi/editor
    clear_msg(),
    Ws:send_text(ws_req([rpc, 'test.add5', [1, 2]])),
    ?assertMatch({text, <<"3">>}, ws_resp()),

    %% stop pp_rabbit
    pp_rabbit:stop(),

    %% logout
    clear_msg(),
    Ws:send_text(ws_req([<<"logout">>])),
    ?assertMatch({text, <<"ok">>}, ws_resp()),

    clear_msg(),
    Ws:send_text(ws_req([<<"status">>])),
    ?assertMatch({text, [<<"offline">>, #{}]}, ws_resp()),

    Ws:close().
