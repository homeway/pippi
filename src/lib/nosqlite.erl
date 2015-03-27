-module(nosqlite).
-export([
    create_schema/1, create_table/2,
    delete_table/1, clear_table/1,
    table/1, size/1,
    create/2, create/3, delete/2, update/3, get/2,
    find/4, find_all/4, find_key/4,
    eq/2, lt/2, gt/2, lte/2, gte/2,
    all/1, all_keys/1]).

-include_lib("stdlib/include/qlc.hrl").

-define(ver, [nosqlite, 0, 1]).

%%% we use mnesia to store nosqlite
%%%
%%% features:
%%% 1) as nosql style, there is no schema must to specified
%%% 2) some meta data like created_at, lastmodified_at
%%% 3) embedded update logs into meta data
%%% 4) json prepare:
%%%      iolist, binary, atom -> string,
%%%      int, list, map
%%% 5) easy crud
%%% 6) mode: ram, disc, disc_only
%%% 7) multi nodes
%%%
%%% data schema:
%%%
%%% {table(), key(), data(), meta()}
%%%   - table(), table name
%%%   - key(), item key in table
%%%   - data(), user data store  with map()
%%%   - meta(), system data store with map():
%%%       created_at, lastmodified_at, update_logs
%%%
%%% you can use tuple module style
%%% T = nosqlite:table(account),
%%% T:all().
%%%
%%% we use a json-firendly style to return data
%%% [key(), data(), meta())]
%%% [[key(), data(), meta()]]
%%%
%%% change log: change table schema with [key(), data(), meta()]
%%%
%%% todo: page/1, all/1 with pagination)
%%% todo: search/2, condition search with pagination
%%% todo: second index
%%% todo: meta info table
%%% todo: disc log

%% init database schema
create_schema(Tables) ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    [T:create_table() || T <- Tables],
    mnesia:stop().

create_table(Tab, disc) -> create_table(Tab, ram_copies);
create_table(Tab, disc_only) -> create_table(Tab, disc_only_copies);
create_table(Tab, ram) -> create_table(Tab, ram_copies);
create_table(Tab, CopyType) ->
    R = mnesia:create_table(Tab,
        [{CopyType, [node()]}, {attributes, [key, map, meta]}]),
    case R of
        {atomic, ok} -> ok;
        {aborted, {already_exists, _}} -> ok;
        Reason -> Reason
    end.

%% delete table
delete_table(Tab) ->
    {atomic, ok} = mnesia:delete_table(Tab), ok.

%% delete all data in table
clear_table(Tab) ->
    {atomic, ok} = mnesia:clear_table(Tab), ok.

%% table info
size({?MODULE, Tab}) ->
    mnesia:table_info(Tab, size).

%% construct a tuple module
table(Tab) -> {?MODULE, Tab}.

%% create an item with map()
create(Data, {?MODULE, Tab}) ->
    K = pp:uuid(),
    case create(K, Data, {?MODULE, Tab}) of
        ok ->    {ok, K};
        Error -> Error
    end.

%% create an item with key() and map()
create(Key, Data, {?MODULE, Tab}) ->
    Item = {Tab, Key, Data, #{
        ver => ?ver,
        created_at => pp:now_to_human(),
        lastmodified_at => pp:now_to_human()
    }},
    {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Item) end), ok.

%% delete an item with key()
delete(Key, {?MODULE, Tab}) ->
    Item = {Tab, Key},
    {atomic, ok} = mnesia:transaction(fun()-> mnesia:delete(Item) end), ok.

%% update an old item() with key() and new property map()
update(Key, NewProp, {?MODULE, Tab}) ->
    case get(Key, {?MODULE, Tab}) of
        not_found -> not_found;
        error -> error;
        multi_records -> multi_records;
        [_Key, Data, Meta] ->
            {D, M} = data_to_update(Data, Meta, NewProp),
            NewItem = {Tab, Key, D, M},
            {atomic, ok} = mnesia:transaction(fun()->mnesia:write(NewItem) end),
            ok
    end.

%% private method for update
data_to_update(Data, Meta, NewProp) ->
    OldProp = prop_to_update(Data, NewProp),
    OldLog = maps:get(update_logs, Meta, []),
    UpdateTime = pp:now_to_human(),
    CreateTime = maps:get(created_at, Meta, UpdateTime),
    D = maps:merge(Data, NewProp),
    M = #{
        created_at => CreateTime,
        update_logs => [[UpdateTime, OldProp, NewProp]|OldLog],
        lastmodified_at => UpdateTime},
    {D, M}.
prop_to_update(Data, NewProp) ->
    L = maps:to_list(NewProp),
    R = [{PropK, maps:get(PropK, Data, undefined)} || {PropK, _} <- L],
    maps:from_list(R).


%% got an item with key()
%% not allowd duplicate key()
get(Key, {?MODULE, Tab}) ->
    Cond = qlc:q([
        X || {_, K0, _, _} = X <- mnesia:table(Tab),
        Key =:= K0
]),
    {atomic, R} = mnesia:transaction(fun()->qlc:e(Cond) end),
    case R of
        [{_, K, D, M}] -> [K, D, M];
        [] -> not_found;
        [_H1|[_H2|_]] -> multi_records;
        _ -> error
    end.

%% find all items with property
find_all(PropK, Op, PropV, {?MODULE, Tab}) ->
    case lists:member(Op, [eq, lt, gt, lte, gte]) of
        true -> Fun = fun ?MODULE:Op/2;
        false when is_function(Op) -> Fun = Op
    end,
    Cond = case lists:member(PropK, [created_at, lastmodified_at, update_logs]) of
        true ->
            qlc:q([
                [Key, Data, Meta] || {_, Key, Data, Meta} <- mnesia:table(Tab),
                Fun(maps:get(PropK, Meta, undefined), PropV)]);
        false ->
            qlc:q([
                [Key, Data, Map] || {_, Key, Data, Map} <- mnesia:table(Tab),
                Fun(maps:get(PropK, Data, undefined), PropV)])
    end,
    {atomic, R} = mnesia:transaction(fun() -> qlc:e(Cond) end), R.

%% find only one item with [key(), map()]
find(PropK, Op, PropV, {?MODULE, Tab}) ->
    case find_all(PropK, Op, PropV, {?MODULE, Tab}) of
        [] -> not_found;
        [R|_] -> R
    end.

%% return only one key uuid()
find_key(PropK, Op, PropV, {?MODULE, Tab}) ->
    case find(PropK, Op, PropV, {?MODULE, Tab}) of
        not_found -> not_found;
        [Key, _, _] -> Key
    end.

%% return all items with [[key(), map()]]
all({?MODULE, Tab}) ->
    Cond = qlc:q([[K, D, M] || {_, K, D, M} <- mnesia:table(Tab)]),
    {atomic, R} = mnesia:transaction(fun()->qlc:e(Cond) end),
    try
        lists:sort(fun({_, _, #{created_at:=C1}}, {_, _, #{created_at:=C2}}) ->
            gt(C1, C2)
        end, R)
    catch
        _:_ -> R
    end.

eq(V1, V2)  -> V1 =:= V2.
lt(V1, V2)  -> V1 < V2.
gt(V1, V2)  -> V1 > V2.
lte(V1, V2) -> V1 =< V2.
gte(V1, V2) -> V1 >= V2.

all_keys({?MODULE, Tab}) ->
    {atomic, R} = mnesia:transaction(fun()->mnesia:all_keys(Tab) end),
    R.
