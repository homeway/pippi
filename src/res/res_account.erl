%% -*- mode: nitrogen -*-
-module(res_account).
-export([init_tables/0, insert/3, all/0]).
-include_lib("stdlib/include/qlc.hrl").

-record(account, {id, name, email, tel}).
-record(online, {id, token, account_id}).

init_tables() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(account,
        [{disc_copies, [node()]}, {attributes, record_info(fields, account)}]),
    mnesia:create_table(online,
        [{ram_copies, [node()]}, {attributes, record_info(fields, online)}]),
    mnesia:stop().

insert(Name, Email, Tel) ->
    Item = #account{id=ss_utils:uuid(), name=Name, email=Email, tel=Tel},
    mnesia:transaction(fun() -> mnesia:write(Item) end).

all() ->
    Tab = account,
    Cond = qlc:q([X || X <- mnesia:table(Tab)]),
    {atomic, R} = mnesia:transaction(fun() -> qlc:e(Cond) end),
    R.
