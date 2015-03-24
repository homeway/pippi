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

    %% 错误的登录
    ?assertMatch({error, _Msg}, A1:login("user", "123")),

    ?assertMatch({offline, _Status}, A1:status()),

    %% 正确的登录
    ?assertMatch(ok, A1:login("adi", "123")),

    A1:status(),
    ?assertMatch({online, #{user := <<"adi">>}}, A1:status()),

    %% exit
    A1:stop().
