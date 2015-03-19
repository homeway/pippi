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
    A1 = pp_account:start(1000, 1000),
    clear_msg(),

    %% 错误信息的登录
    A1:login("user", "123"),
    ?assertMatch({login, _Ref, {error, _Msg}}, got_msg()),
    A1:stop().



