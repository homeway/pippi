%% -*- mode: nitrogen -*-
-module(pp_account_test).
-include_lib("eunit/include/eunit.hrl").

%% clear_msg() ->
%%     receive _ -> clear_msg()
%%     after 50 -> ok
%%     end.

%% got_msg() ->
%%     receive Msg -> Msg
%%     after 50 -> nothing
%%     end.

login_test() ->
    %% start
    A1 = pp_account:start(1000, 1000),

    %% 未登录时的错误操作
    ?assertMatch({error, offline, no_this_action}, A1:logout()),

    %% 获取授权更新
    ?assertMatch([[users, [all, get]]], A1:methods()),

    %% 错误的登录
    ?assertMatch({error, _Msg}, A1:login("user", "123")),
    ?assertMatch({offline, _Status}, A1:status()),

    %% 正确的登录
    ?assertMatch(ok, A1:login("adi", "123")),
    ?assertMatch({online, #{user := <<"adi">>}}, A1:status()),

    %% 获取授权更新
    ?assertMatch([users], A1:methods()),

    %% 已登录时的错误操作
    ?assertMatch({error, online, no_this_action}, A1:login("adi", "123")),

    %% 注销
    ?assertMatch(ok, A1:logout()),
    ?assertMatch({offline, _Status}, A1:status()),

    %% exit
    A1:stop().
