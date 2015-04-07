%% -*- mode: nitrogen -*-
%%%-------------------------------------------------------------------
%%% @author homeway <homeway.xue@gmail.com>
%%% @copyright (C) 2015, homeway
%%% @doc
%%% pp_odbc is a module to access oracle
%%%
%%% issues:
%%%   1) sql cann't contain tail ';'
%%%   2) the odbc:describe_table/2 does'nt work
%%%      because desc method not in oracle server but in sqlplus
%%%
%%% @end
%%% Created :  7 Apr 2015 by homeway <homeway.xue@gmail.com>
%%%-------------------------------------------------------------------
-module(pp_odbc_lib).
-export([open/3, query/2, exec_sql/2, close/1, desc/2,
    count/2, count/3, page/2, page/3, page/4, all/2]).

% open() ->
%     open(<<"myebig">>, <<"tayy">>, <<"tayy">>).

open(DSN, UID, PWD) ->
    case odbc:connect(io_lib:format("DSN=~s;UID=~s;PWD=~s",
        [pp:to_binary(DSN), pp:to_binary(UID), pp:to_binary(PWD)]),
        [{binary_strings, on}]) of
        {ok, Ref} ->
            {?MODULE, Ref};
        Error -> Error
    end.

exec_sql(Sql, {?MODULE, Ref}) ->
    case odbc:sql_query(Ref, Sql) of
        {error, Reason} when is_atom(Reason)->
            Reason;
        {error, Reason} ->
            io:format("~ts~n", [list_to_binary(Reason)]),
            Reason;
        R -> R
    end.

%% return [map()]
query(Sql, {?MODULE, Ref}) ->
    return_maps(exec_sql(Sql, {?MODULE, Ref})).

%% json prepared by return [map()]
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
    Sql = io_lib:format(
        "select count(1) from (~ts)", [pp:to_binary(Sql0)]),
    {selected, _, [{C}]} = odbc:sql_query(Ref, Sql),
    Count = binary_to_integer(C),
    Pages = case Count rem Size of
        0 -> Count div Size;
        _ -> Count div Size + 1
    end,
    {Count, Pages, Size}.

page(Sql,{?MODULE, Ref}) ->
    page(Sql, 30, 1, {?MODULE, Ref}).

page(Sql, Num, {?MODULE, Ref}) ->
    page(Sql, 30, Num, {?MODULE, Ref}).

page(Sql, Num, Size, {?MODULE, Ref}) ->
    From = Size * (Num - 1) + 1,
    To = From + Size - 1,
    PageSql = io_lib:format(
        "SELECT * FROM (SELECT A.*, ROWNUM RN FROM (~ts) A WHERE ROWNUM <= ~B) WHERE RN >= ~B",
        [pp:to_binary(Sql), To, From]),
    io:format("~ts", [PageSql]),
    query(PageSql, {?MODULE, Ref}).

desc(Table0, {?MODULE, Ref}) ->
    Table = if
        is_list(Table0) -> string:to_upper(Table0);
        true -> Table0
    end,
    Sql = io_lib:format("select decode( t.table_name, lag(t.table_name, 1) over(order by t.table_name), null, t.table_name ) as table_name, t.column_name, t.data_type, cc.constraint_name, uc.constraint_type from user_tab_columns t left join user_cons_columns cc on (cc.table_name = t.table_name and cc.column_name = t.column_name) left join user_constraints uc on (t.table_name = uc.table_name and uc.constraint_name = cc.constraint_name ) where t.table_name in ('~s')", [pp:to_binary(Table)]),
    query(Sql, {?MODULE, Ref}).

all(Table, {?MODULE, Ref}) ->
    Sql = io_lib:format("select * from ~s", [pp:to_binary(Table)]),
    query(Sql, {?MODULE, Ref}).

close({?MODULE, Ref}) ->
    odbc:disconnect(Ref).


%% 从actul odbc驱动 访问oracle时有一个bug（未得到官方证实）
%% oracle的varchar2默认以byte为单位，汉字为2个byte表示
%% 但erlang/odbc则以3个byte表示，例如字段为18个汉字，长度限制为50 bytes
%% 则erlang/odbc读取50bytes后截断，而oracle则可正常存储25个汉字
%%
%% 不完整的utf8编码会引起jiffy:encode异常或字符串使用乱码
%% 这里采用临时解决方案，即截断多余字符，保障utf8编码可正常解析
%% 试探方式是采用jiffy:encode转换，分页查询的情况下效率完全可以接受
to_maps_type(L) when is_list(L) -> list_to_binary(L);
to_maps_type(B) when is_binary(B) ->
    Size = size(B),
    if
        Size > 3 ->
            try jiffy:encode(B) of
                _ -> B
            catch
                throw:{error,{invalid_string, _}} ->
                    Size1 = Size - 1,
                    <<B1:Size1/binary, _/binary>> = B,
                    try jiffy:encode(B1) of
                        _ -> B1
                    catch
                        throw:{error,{invalid_string, _}} ->
                            Size2 = Size - 2,
                            <<B2:Size2/binary, _/binary>> = B,
                            B2
                    end
            end;
        true -> B
    end;

to_maps_type(Other) -> Other.
