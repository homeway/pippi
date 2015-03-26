%% -*- mode: nitrogen -*-
-module(nosqlite_test).
-include_lib("eunit/include/eunit.hrl").

table_test() ->
    mnesia:delete_table(users),
    ?assertEqual(ok, nosqlite:create_table(users, ram)),
    %% support confirm create
    ?assertEqual(ok, nosqlite:create_table(users, ram)),

    T = nosqlite:table(users),

    %% crud
    ?assertEqual([], T:all()),
    ?assertEqual(not_found, T:get(2)),

    {ok, K1} = T:create(#{name=>adi, age=>36}),
    ?assertMatch([K1, #{name:=adi}, _], T:get(K1)),

    ?assertEqual(ok, T:create(1, #{name=>yifan, age=>10})),
    ?assertMatch([1, #{name:=yifan}, _], T:get(1)),

    ?assertEqual(ok, T:update(1, #{name=>erfan})),
    ?assertMatch([1, #{name:=erfan}, _], T:get(1)),
    ?assertEqual(2, T:size()),
    ?assertEqual(lists:sort([1,K1]), lists:sort(T:all_keys())),
    ?assertMatch([[1, #{name:=erfan}, _], [K1, _, _]], lists:sort(T:all())),

    %% search item
    ?assertMatch([K1, #{name:=adi}, _], T:find(name, eq, adi)),
    ?assertMatch([K1, #{name:=adi}, _], T:find(name, fun(V1, V2) -> V1 =:= V2 end, adi)),

    %% search key
    ?assertEqual(K1, T:find_key(name, eq, adi)),

    %% search items
    T:create(#{name=>wenxuan, age=>10}),
    T:create(#{name=>xiaoije, age=>11}),
    ?assertEqual(2, length(T:find_all(age, eq, 10))),
    ?assertEqual(2, length(T:find_all(age, gte, 11))),

    %% delete
    ?assertEqual(ok, T:delete(1)),
    ?assertEqual(3, T:size()).
