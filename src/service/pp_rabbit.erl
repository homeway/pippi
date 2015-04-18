%% -*- mode: nitrogen -*-
%%%-------------------------------------------------------------------
%%% @author homeway <homeway.xue@gmail.com>
%%% @copyright (C) 2015, homeway
%%% @doc
%%%   init rpc call methods over amqp
%%%   1) all rpc server must support json string as only param
%%%       example: rpc_service(<<"[1, 2]">>)
%%%   2) all rpc server must return json string
%%%       example: rpc_server(<<"[1, 2]">>) -> <<"[3, {step1: 1, step2: 2}]">>
%%% @end
%%% Created :  7 Apr 2015 by homeway <homeway.xue@gmail.com>
%%%---------------------------------------------------------------
-module(pp_rabbit).

-behaviour(gen_server).

%% API
-export([start/0, stop/0, start_link/0, rpc_call/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

-define(SERVER, ?MODULE).

%%%===============================================================
%%% API
%%%===============================================================

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

init(_) ->
    Network = #amqp_params_network{host="localhost"},
    {ok, Conn} = amqp_connection:start(Network),

    Queues = [],

    List1 = [
        {pp:to_binary(Q), amqp_rpc_client:start_link(Conn, Q)} ||
        Q <- Queues
    ],
    {ok, #{connect => Conn, queues => maps:from_list(List1)}}.

%% register a rpc server:
%% the params and returned data must compatible with jiffy json

%% register an erlang rpc server
reg_rpc_server(Queue, Func) when is_function(Func)->
    ok.
reg_rpc_server(Queue, M, F, Count) ->
    ok.
unreg_rpc_server(Queue) ->
    ok.

%% register all functions in module as rpc server
reg_rpc_module(M) ->
    ok.
unreg_rpc_module(M) ->
    ok.

%% register a temperory client
reg_rpc_client(Queue) ->
    ok.
unreg_rpc_client(Queue) ->
    ok.

%% prepare to auto register rpc server by erlang
reg_auto_rpc_server(Queue, M, F, Count) ->
    ok.
unreg_auto_rpc_server(Queue) ->
    ok.
reg_auto_rpc_module(M) ->
    ok.
unreg_auto_rpc_module(M) ->
    ok.

%% prepare to auto register rpc client
%% the rpc service can be supply by none-erlang process
reg_auto_rpc_client(Queue) ->
    ok.
unreg_auto_rpc_client(Queue) ->
    ok.

%% rpc client call
rpc_call(Queue, Params) ->
    gen_server:call(?SERVER, {rpc_call, Queue, Params}).

%%%===============================================================
%%% gen_server callback
%%%===============================================================

handle_call(status, _F, S) -> {reply, S, S};
handle_call({rpc_call, Q, P}, _F, #{connect:=Conn, queues:=Queues}=S) ->
    case maps:get(pp:to_binary(Q), Queues, undefined) of
        undefined -> {reply, undefined, S};
        Client -> {reply, amqp_rpc_client:call(Client, P), S}
    end.

handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, #{connect:=Conn, queues:=Queues }) ->
    List1 = maps:to_list(Queues),
    [amqp_rpc_client:stop(Client) || {_Q, Client} <- List1],
    amqp_connection:close(Conn),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
