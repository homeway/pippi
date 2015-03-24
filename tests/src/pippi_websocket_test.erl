%% -*- mode: nitrogen -*-
-module(pippi_websocket_test).
-include_lib("eunit/include/eunit.hrl").

clear_msg() ->
    receive
        _Msg -> clear_msg()
    after
        50 -> ok
    end.

got_msg() ->
    receive
        Msg -> Msg
    after
        50 -> nothing
    end.

ws_req(Cmd) ->
    jiffy:encode(Cmd).

ws_resp() ->
    {text, R} = got_msg(),
    {text, jiffy:decode(R, [return_maps])}.

main_test() ->
    Ws = test_ws_handler:connect("ws://127.0.0.1:8080/ws"),

    %% anonymouse
    clear_msg(),
    Ws:send_text(ws_req([status])),
    ?assertMatch({text, [<<"offline">>, #{}]}, ws_resp()),

    %% login
    clear_msg(),
    Ws:send_text(ws_req([login, ["adi", "123"]])),
    ?assertEqual({text, [<<"ok">>]}, ws_resp()),

    clear_msg(),
    Ws:send_text(ws_req([status])),
    ?assertMatch({text, [<<"online">>, #{}]}, ws_resp()),

    Ws:close().
