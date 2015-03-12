-module(pp_nosqlite).
-export([
  create_schema/1, create_table/2, table/1,
  create/2, create/3, delete/2, update/3, get/2,
  find/3, find_all/3, find_key/3,
  all/1]).

-include_lib("stdlib/include/qlc.hrl").

%%% we use mnesia to store nosqlite
%%% 
%%% {table(), key(), map()}
%%%
%%% table() - table name
%%% key()   - item key in table
%%% map()   - data store in table with map()
%%%           data map include some property auto generated:
%%%           created_at, lastmodified_at, update_logs
%%%
%%%
%%% above is the data schema of all data schema
%%%
%%% you can use tuple module style
%%% T = pp_nosqlite:table(account),
%%% T:all().
%%%
%%% we use a json-firendly style to return data
%%% [key(), map()]
%%% [[key(), map()]]


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
  mnesia:create_table(Tab,
    [{CopyType, [node()]}, {attributes, [key, map]}]).

%% construct a tuple module
table(Tab) -> {?MODULE, Tab}.

%% create an item with map()
create(Data, {?MODULE, Tab}) -> create(pp:uuid(), Data, {?MODULE, Tab}).

%% create an item with key() and map()
create(Key, Data, {?MODULE, Tab}) ->
  Item = {Tab, Key, Data#{
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
    [_Key, Data] ->
      NewItem = {Tab, Key, data_to_update(Data, NewProp)},
      {atomic, ok} = mnesia:transaction(fun()->mnesia:write(NewItem) end),
      ok
  end.

%% private method for update
data_to_update(Data, NewProp) ->
  OldProp = prop_to_update(Data, NewProp),
  OldLog = maps:get(update_logs, Data, []),
  UpdateTime = pp:now_to_human(),
  NewLog = [[UpdateTime, OldProp, NewProp]|OldLog],
  maps:merge(Data, NewProp#{
    update_logs => NewLog,
    lastmodified_at => UpdateTime}).
prop_to_update(Data, NewProp) ->
  L = maps:to_list(NewProp),
  R = [{PropK, maps:get(PropK, Data, undefined)} || {PropK, _} <- L],
  maps:from_list(R).


%% got an item with key()
%% not allowd duplicate key()
get(Key, {?MODULE, Tab}) ->
    Cond = qlc:q([
        X || {_, K0, _} = X <- mnesia:table(Tab),
        Key =:= K0
    ]),
    {atomic, R} = mnesia:transaction(fun()->qlc:e(Cond) end),
    case R of
      [{_, K, M}] -> [K, M];
      [] -> not_found;
      [_H1|[_H2|_]] -> multi_records;
      _ -> error
    end.

%% find all items with property
find_all(PropK, PropV, {?MODULE, Tab}) ->
  Cond = qlc:q([
    [Key, Map] || {_, Key, Map} <- mnesia:table(Tab),
    maps:get(PropK, Map, undefined) =:= PropV]),
  {atomic, R} = mnesia:transaction(fun() -> qlc:e(Cond) end), R.

%% find only one item with [key(), map()]
find(PropK, PropV, {?MODULE, Tab}) ->
  case find_all(PropK, PropV, {?MODULE, Tab}) of
    [] -> not_found;
    [R|_] -> R
  end.

%% return only one key uuid()
find_key(PropK, PropV, {?MODULE, Tab}) ->
  case find(PropK, PropV, {?MODULE, Tab}) of
    not_found -> not_found;
    [Key, _] -> Key
  end.

%% return all items with [[key(), map()]]
all({?MODULE, Tab}) ->
    Cond = qlc:q([[K, M] || {_, K, M} <- mnesia:table(Tab)]),
    {atomic, R} = mnesia:transaction(fun()->qlc:e(Cond) end),
    try
      lists:sort(fun({_, #{created_at:=C1}}, {_, #{created_at:=C2}}) ->
          C1 > C2
      end, R)
    catch
      _:_ -> R
    end.
