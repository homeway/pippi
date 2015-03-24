%% -*- mode: nitrogen -*-
-module(pippi_websocket_test).
-include_lib("eunit/include/eunit.hrl").

got_msg() ->
    receive
        Msg -> Msg
    after 500 -> nothing end.


main_test() ->
    Ws = test_ws_handler:connect("ws://127.0.0.1:8080/ws"),
    Ws:send_text(jiffy:encode([hello])),
    ?assertEqual({text, jiffy:encode([processing])}, got_msg()),
    Ws:close().
