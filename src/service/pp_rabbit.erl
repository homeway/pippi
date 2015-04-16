%% -*- mode: nitrogen -*-
%%%-------------------------------------------------------------------
%%% @author homeway <homeway.xue@gmail.com>
%%% @copyright (C) 2015, homeway
%%% @doc
%%%
%%% @end
%%% Created :  7 Apr 2015 by homeway <homeway.xue@gmail.com>
%%%-------------------------------------------------------------------
-module(pp_rabbit).

-behaviour(gen_server).

%% API
-export([start/0, stop/0, start_link/0]).
-export([status/0, reg/2, call/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

init([]) ->
    {M, Conn0} = Conn = pp_rabbit_lib:connect("localhost"),
    {M, Ch0} = Ch = Conn:channel(),
    link(Conn0),
    link(Ch0),

    {ok, #{connect => Conn, default_channel => Ch}}.

status()   -> gen_server:call(?SERVER, status).
call(F, A) -> gen_server:call(?SERVER, {call, F, A}).
reg(RpcName, Func)  -> gen_server:call(?SERVER, {reg, RpcName, Func}).

%%%===================================================================
%%% gen_server callback
%%%===================================================================

handle_call(status, _F, S) -> {reply, S, S};
handle_call({reg, RpcName, Func}, _F, #{default_channel:=Ch}=S) ->
    {reply, Ch:rpc_reg(RpcName, Func), S};
handle_call({call, F, A}, _F, #{default_channel:=Ch}=S) ->
    {_Seq, R} = Ch:rpc_call(F, A),
    {reply, R, S}.

handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, #{connect := Conn, default_channel:=Ch}) ->
    Ch:close_channel(),
    Conn:disconnect(),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
