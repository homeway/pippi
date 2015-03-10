-module(res_sms_records).
-export([init_tables/0]).
-export([insert/1, get/1, update/2, delete/1, all/0]).
-include_lib("stdlib/include/qlc.hrl").

init_tables() ->
    mnesia:create_table(sms_records,
        [{disc_copies, [node()]}, {attributes, [id, data]}]).

insert(Record) ->
    Id = ss_utils:uuid(),
    Item = {sms_records, Id, Record#{
        created_at => ss_time:now_to_human(),
        lastmodified_at => ss_time:now_to_human()
    }},
    {atomic, ok} = mnesia:transaction(fun()->mnesia:write(Item) end),
    {ok, Id}.

get(Id) ->
    Tab = sms_records,
    Cond = qlc:q([
        X || {_, Id0, _} = X <- mnesia:table(Tab),
        Id =:= Id0
    ]),
    {atomic, R} = mnesia:transaction(fun()->qlc:e(Cond) end),
    R.

update(Id, Record) ->
    Item = {sms_records, Id, Record#{
        lastmodified_at => ss_time:now_to_human()
    }},
    mnesia:transaction(fun()->mnesia:write(Item) end).

delete(Id) ->
    Item = {sms_records, Id},
    mnesia:transaction(fun()->mnesia:delete(Item) end).

all() ->
    Tab = sms_records,
    Cond = qlc:q([X || X <- mnesia:table(Tab)]),
    {atomic, R} = mnesia:transaction(fun()->qlc:e(Cond) end),
    lists:sort(fun({_, _, #{created_at:=C1}}, {_, _, #{created_at:=C2}}) ->
        C1 > C2
    end, R).
