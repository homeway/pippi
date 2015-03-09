%% -*- mode: nitrogen -*-
-module(res_account).
-export([init_tables/0, insert/1, all/0, check_pass/2, to_test/0]).
-include_lib("stdlib/include/qlc.hrl").

-record(account, {name, email}).
to_test() ->
    erlang:display({record_info, record_info(fields, account)}).

%% There is no record here, use map() to store data in mnesia
%% so we create table directly use mnesia:create_table(TableName, [{attributes, []}])
%%
%% table account
%% {account, id(), #{id(), name(), email(), tel(), pass()}}
%%
%% table online
%% {online, token(), #{account_id(), account_name()}}

init_tables() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(account,
        [{disc_copies, [node()]}, {attributes, [id, data]}]),
    mnesia:create_table(online,
        [{ram_copies, [node()]}, {attributes, [token, data]}]),
    mnesia:stop().

insert(Account) ->
    Item = {account, ss_utils:uuid(), Account},
    mnesia:transaction(fun() -> mnesia:write(Item) end).

all() ->
    Tab = account,
    Cond = qlc:q([X || X <- mnesia:table(Tab)]),
    {atomic, R} = mnesia:transaction(fun() -> qlc:e(Cond) end),
    R.

check_pass(Name0, Pass0) ->
    Cond = qlc:q([
        Name1 || {_, _, #{name:=Name1, pass:=Pass1}} <- mnesia:table(account),
        Name0=:=Name1,
        Pass0=:=Pass1]),
    {atomic, R} = mnesia:transaction(fun() -> qlc:e(Cond) end),
    length(R) > 0.
