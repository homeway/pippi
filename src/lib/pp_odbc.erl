%% -*- mode: nitrogen -*-
-module(pp_odbc).
-export([open/0, open/3, query/2, close/1, desc/2,
    count/2, count/3, page/2, page/3, page/4, all/2]).

open() ->
    open(<<"myebig">>, <<"tayy">>, <<"tayy">>).

open(DSN, UID, PWD) ->
    case odbc:connect(io_lib:format("DSN=~s;UID=~s;PWD=~s",
        [pp:to_binary(DSN), pp:to_binary(UID), pp:to_binary(PWD)]), []) of
        {ok, Ref} ->
            {?MODULE, Ref};
        Error -> Error
    end.

query(Sql, {?MODULE, Ref}) ->
    case odbc:sql_query(Ref, Sql) of
        {error, Reason} when is_atom(Reason)->
            Reason;
        {error, Reason} ->
            io:format("~ts~n", [list_to_binary(Reason)]),
            Reason;
        R -> return_maps(R)
    end.

return_maps({selected, Columns0, Rows})->
    Columns = [to_maps_type(C) || C <- Columns0],
    lists:map(fun(Row0) ->
        Row = [to_maps_type(F) || F <- tuple_to_list(Row0)],
        if
            length(Columns) =:= length(Row) ->
                maps:from_list(return_map(Columns, Row, []));
            true ->
                erlang:display(Columns),
                erlang:display(Row),
                #{}
        end
    end, Rows).

return_map([], [], Acc) -> Acc;
return_map([Field|Fields], [Value|Values], Acc) ->
    return_map(Fields, Values, [{Field, Value}|Acc]).

count(Sql, {?MODULE, Ref}) ->
    count(Sql, 30, {?MODULE, Ref}).
count(Sql0, Size, {?MODULE, Ref}) ->
    Sql = io_lib:format("select count(1) from (~ts)", [pp:to_binary(Sql0)]),
    {selected, _, [{C}]} = odbc:sql_query(Ref, Sql),
    Count = list_to_integer(C),
    Pages = case Count rem Size of
        0 -> Count div Size;
        _ -> Count div Size + 1
    end,
    {Count, Pages, Size}.

page(Sql,{?MODULE, Ref}) ->
    page(Sql, 30, 1, {?MODULE, Ref}).

page(Sql, Num, {?MODULE, Ref}) ->
    page(Sql, 30, Num, {?MODULE, Ref}).

page(Sql, Size, Num, {?MODULE, Ref}) ->
    From = Size * (Num - 1) + 1,
    To = From + Size - 1,
    PageSql = io_lib:format("SELECT * FROM (SELECT A.*, ROWNUM RN FROM (~ts) A WHERE ROWNUM <= ~B) WHERE RN >= ~B", [pp:to_binary(Sql), To, From]),
    query(PageSql, {?MODULE, Ref}).

desc(Table, {?MODULE, Ref}) ->
    Sql = io_lib:format("select decode( t.table_name, lag(t.table_name, 1) over(order by t.table_name), null, t.table_name ) as table_name, t.column_name, t.data_type, cc.constraint_name, uc.constraint_type from user_tab_columns t left join user_cons_columns cc on (cc.table_name = t.table_name and cc.column_name = t.column_name) left join user_constraints uc on (t.table_name = uc.table_name and uc.constraint_name = cc.constraint_name ) where t.table_name in ('~s')", [pp:to_binary(Table)]),
    query(Sql, {?MODULE, Ref}).

all(Table, {?MODULE, Ref}) ->
    Sql = io_lib:format("select * from ~s", [pp:to_binary(Table)]),
    query(Sql, {?MODULE, Ref}).

close({?MODULE, Ref}) ->
    odbc:disconnect(Ref).


to_maps_type(I) when is_integer(I) -> I;
to_maps_type(L) when is_list(L)-> list_to_binary(L);
to_maps_type(Other) -> Other.
