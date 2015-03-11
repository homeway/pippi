-module(pp_nosqlite).
-export([
  create_schema/1, init/2, table/1,
  create/2, create/3, delete/2, update/3, get/2,
  find/3, find_all/3, find_key/3,
  all/1]).

-include_lib("stdlib/include/qlc.hrl").

%%% we use mnesia to store nosqlite
%%% 
%%% {table(), key(), map()}
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
  [T:init() || T <- Tables],
  mnesia:stop().

init(Tab, disc) -> init(Tab, ram_copies);
init(Tab, disc_only) -> init(Tab, disc_only_copies);
init(Tab, ram) -> init(Tab, ram_copies);
init(Tab, CopyType) ->
  mnesia:create_table(Tab,
    [{CopyType, [node()]}, {attributes, [key, map]}]).


table(Tab) -> {?MODULE, Tab}.

create(Data, {?MODULE, Tab}) -> create(pp:uuid(), Data, {?MODULE, Tab}).

create(Key, Data, {?MODULE, Tab}) ->
  Item = {Tab, Key, Data#{
    created_at => pp:now_to_human(),
    lastmodified_at => pp:now_to_human()
  }},
  {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Item) end), ok.

delete(Key, {?MODULE, Tab}) ->
  Item = {Tab, Key},
  {atomic, ok} = mnesia:transaction(fun()-> mnesia:delete(Item) end), ok.

update(Key, Map, {?MODULE, Tab}) ->
  Item = {Tab, Key, Map#{
    lastmodified_at => pp:now_to_human()
  }},
  {atomic, ok} = mnesia:transaction(fun()->mnesia:write(Item) end), ok.

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

find_all(K, V, {?MODULE, Tab}) ->
  Cond = qlc:q([
    [Key, Map] || {_, Key, Map} <- mnesia:table(Tab),
    maps:get(K, Map, undefined) =:= V]),
  {atomic, R} = mnesia:transaction(fun() -> qlc:e(Cond) end), R.

find(K, V, {?MODULE, Tab}) ->
  case find_all(K, V, {?MODULE, Tab}) of
    [] -> not_found;
    [R|_] -> R
  end.

find_key(K, V, {?MODULE, Tab}) ->
  case find(K, V, {?MODULE, Tab}) of
    not_found -> not_found;
    [Key, _] -> Key
  end.

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
