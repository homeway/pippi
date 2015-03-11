%% -*- mode: nitrogen -*-
-module(res_account).
-export([init_tables/0, insert/1, all/0, get/1, delete/1, update/2, find_id/2, find_ids/2]).
-export([update_pass/2, check_pass/2, generate_token/1, token_info/1, delete_token/1]).
-include_lib("stdlib/include/qlc.hrl").

%% There is no record here, use map() to store data in mnesia
%% so we create table directly use mnesia:create_table(TableName, [{attributes, []}])
%%
%% table account
%% {account, id(), #{id(), name(), email(), tel(), pass()}}
%%
%% table online
%% {online, token(), #{account_name()}}

init_tables() ->
    mnesia:create_table(account,
        [{disc_copies, [node()]}, {attributes, [id, data]}]),
    mnesia:create_table(online,
        [{ram_copies, [node()]}, {attributes, [token, data]}]).

insert(Account) ->
    Item = {account, ss_utils:uuid(), Account},
    mnesia:transaction(fun() -> mnesia:write(Item) end).

get(AccountName) ->
    Cond = qlc:q([
        {Id, Data} || {_, Id, Data} <- mnesia:table(account),
        AccountName =:= maps:get(name, Data, undefined)]),
    {atomic, R} = mnesia:transaction(fun()->qlc:e(Cond) end),
    R.

delete(Id) ->
    Item = {account, Id},
    {atomic, ok} = mnesia:transaction(fun()-> mnesia:delete(Item) end).

update(Id, Account) ->
    Cond = qlc:q([
        Data || {_, Id0, Data} <- mnesia:table(account),
        Id0 =:= Id]),
    case mnesia:transaction(fun()->qlc:e(Cond) end) of
        {atomic, []} ->
            {error, not_exist};
        {atomic, [Data0|_]} ->
            Item = {account, Id, maps:merge(Data0, Account)},
            mnesia:transaction(fun()-> mnesia:write(Item) end);
        Error ->
            Error
    end.

all() ->
    Tab = account,
    Cond = qlc:q([X || X <- mnesia:table(Tab)]),
    {atomic, R} = mnesia:transaction(fun() -> qlc:e(Cond) end),
    R.

check_pass(Name0, Pass0) ->
    Tab = account,
    Cond = qlc:q([
        Name1 || {_, _, #{name:=Name1, pass:=Pass1}} <- mnesia:table(Tab),
        ss_convert:to_binary(Name0)=:=ss_convert:to_binary(Name1),
        ss_convert:to_binary(Pass0)=:=ss_convert:to_binary(Pass1)]),
    {atomic, R} = mnesia:transaction(fun() -> qlc:e(Cond) end),
    length(R) > 0.

find_ids(K, V) ->
    Tab = account,
    Cond = qlc:q([
        Id || {_, Id, Map} <- mnesia:table(Tab),
        ss:to_binary(maps:get(K, Map, undefined)) =:= ss:to_binary(V)]),
    {atomic, R} = mnesia:transaction(fun() -> qlc:e(Cond) end), R.

find_id(K, V) ->
    case find_ids(K, V) of
        [Key|_] -> Key;
        _ -> not_found
    end.

update_pass(Name, Pass) ->
    Key = find_id(name, Name),
    update(Key, #{pass=>Pass}).

generate_token(AccountName) ->
    Token = ss_utils:uuid(),
    Item = {online, Token, AccountName},
    {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Item) end),
    Token.

token_info(Token) ->
    Cond = qlc:q([
        AccountName || {_, Token0, AccountName} <- mnesia:table(online),
        Token0 =:= Token]),
    case mnesia:transaction(fun() -> qlc:e(Cond) end) of
        {atomic, []} ->
            {error, token_not_exist};
        {atomic, [Name]} ->
            {ok, Name}
    end.

delete_token(Token) ->
    Item = {online, Token},
    {atomic, ok} = mnesia:transaction(fun() -> mnesia:delete(Item) end).
