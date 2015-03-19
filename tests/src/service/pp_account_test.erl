-module(pp_account_test).
-include_lib("eunit/include/eunit.hrl").

clear_msg() ->
    receive _ -> clear_msg()
    after 50 -> ok
    end.

got_msg() ->
    receive Msg -> Msg
    after 50 -> nothing
    end.

main_test() ->
    %% start
    A1 = pp_account:start(1000, 1000),

    %% 发送离线通知
    clear_msg(),
    A1:notify("hello"),
    ?assertMatch({notify, _Ref, {ok, offline}}, got_msg()),

    %% 错误的登录
    clear_msg(),
    A1:login("user", "123"),
    ?assertMatch({login, _Ref, {error, _Msg}}, got_msg()),

    %% 正确的登录
    clear_msg(),
    A1:login("adi", "123"),
    ?assertMatch({login, _Ref, ok}, got_msg()),
    %% slots群发
    ?assertMatch({online, _Ref, <<"adi">>}, got_msg()),

    %% 发送在线通知
    clear_msg(),
    A1:notify("hello"),
    ?assertMatch({notify, _Ref, {ok, online}}, got_msg()),
    %% slots群发
    ?assertMatch({notify, _Ref, "hello"}, got_msg()),

    %% exit
    A1:stop().



