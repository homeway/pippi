%% -*- mode: nitrogen -*-
%%%-------------------------------------------------------------------
%%% @author homeway <homeway.xue@gmail.com>
%%% @copyright (C) 2015, homeway
%%% @doc
%%%
%%% @end
%%% Created :  7 Apr 2015 by homeway <homeway.xue@gmail.com>
%%%-------------------------------------------------------------------
-module(pp_odbc).

-behaviour(gen_server).

%% API
-export([start/3, stop/0, start_link/3]).
-export([query/1, exec_sql/1, desc/1,
    count/1, count/2, page/1, page/2, page/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start(DSN, UID, Pass) ->
    gen_server:start({local, ?SERVER}, ?MODULE, [DSN, UID, Pass], []).

start_link(DSN, UID, Pass) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [DSN, UID, Pass], []).

stop() ->
    gen_server:cast(?SERVER, stop).

init([DSN, UID, Pass]) ->
    case pp_odbc_lib:open(DSN, UID, Pass) of
        {pp_odbc_lib, Ref} ->
            link(Ref),
            {ok, #{tab => {pp_odbc_lib, Ref}}};
        Reason ->
            {stop, Reason}
    end.

exec_sql(Sql)        -> gen_server:call(?SERVER, {exec_sql, Sql}).
query(Sql)           -> gen_server:call(?SERVER, {'query', Sql}).
count(Sql)           -> gen_server:call(?SERVER, {count, Sql}).
count(Sql, Size)     -> gen_server:call(?SERVER, {count, Size, Sql}).
page(Sql)            -> gen_server:call(?SERVER, {page, Sql}).
page(Sql, Num)       -> gen_server:call(?SERVER, {page, Num, Sql}).
page(Sql, Size, Num) -> gen_server:call(?SERVER, {page, Size, Num, Sql}).
desc(Tab)            -> gen_server:call(?SERVER, {desc, Tab}).

%%%===================================================================
%%% gen_server callback
%%%===================================================================

handle_call({exec_sql, Sql}, _F, #{tab:=T}=S)        -> {reply, T:exec_sql(Sql), S};
handle_call({'query', Sql}, _F, #{tab:=T}=S)         -> {reply, T:query(Sql), S};
handle_call({count, Sql}, _F, #{tab:=T}=S)           -> {reply, T:count(Sql), S};
handle_call({count, Size, Sql}, _F, #{tab:=T}=S)     -> {reply, T:count(Sql, Size), S};
handle_call({page, Sql}, _F, #{tab:=T}=S)            -> {reply, T:page(Sql), S};
handle_call({page, Num, Sql}, _F, #{tab:=T}=S)       -> {reply, T:page(Sql, Num), S};
handle_call({page, Size, Num, Sql}, _F, #{tab:=T}=S) -> {reply, T:page(Size, Num, Sql), S};
handle_call({desc, Tab}, _F, #{tab:=T}=S)            -> {reply, T:desc(Tab), S}.

handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, #{tab:=T}) -> T:close(), ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
