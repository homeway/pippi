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
-export([reg_rpc_server/2, reg_rpc_server/4, unreg_rpc_server/1,
    reg_rpc_client/1, unreg_rpc_client/1,
    reg_auto_rpc_server/4, unreg_auto_rpc_server/1,
    reg_auto_rpc_client/1, unreg_auto_rpc_client/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

-define(AMQP_CLIENT_TEST_CONNECTION_TYPE, "network").
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
    Params = make_network_params([]),
    {ok, Conn} = amqp_connection:start(Params),
    case erlang:is_process_alive(Conn) of
        false -> {stop, connect_to_amqp_failed};
        true ->
            link(Conn),
            pp:display({amqp_connection, Conn}),
            {ok, #{connect => Conn, clients => #{}, servers => #{}}}
    end.

%% register a rpc server:
%% the params and returned data must compatible with jiffy json

%% register an erlang rpc server
reg_rpc_server(Q, Func) when is_function(Func)->
    gen_server:call(?SERVER, {reg_rpc_server, Q, Func}).
reg_rpc_server(Q, M, F, Count) ->
    gen_server:call(?SERVER, {reg_rpc_server, Q, fun M:F/Count}).
unreg_rpc_server(Q) ->
    gen_server:call(?SERVER, {unreg_rpc_server, Q}).

%% register a temperory client
reg_rpc_client(Q) ->
    gen_server:call(?SERVER, {reg_rpc_client, Q}).
unreg_rpc_client(Q) ->
    gen_server:call(?SERVER, {unreg_rpc_client, Q}).

%% prepare to auto register rpc server by erlang
reg_auto_rpc_server(Q, M, F, Count) ->
    ok.
unreg_auto_rpc_server(Q) ->
    ok.

%% prepare to auto register rpc client
%% the rpc service can be supply by none-erlang process
reg_auto_rpc_client(Q) ->
    ok.
unreg_auto_rpc_client(Q) ->
    ok.

%% rpc client call
rpc_call(Q, Params) ->
    gen_server:call(?SERVER, {rpc_call, Q, Params}).

%%%===============================================================
%%% gen_server callback
%%%===============================================================

handle_call(status, _F, S) -> {reply, S, S};

handle_call({reg_rpc_server, Q, Func}, _F, #{connect:=Conn, servers:=Servers}=S) ->
    RPCHandler = fun(X) ->
        try jiffy:encode(apply(Func, jiffy:decode(X))) of
            Res1 -> Res1
        catch
            _:_ -> <<"error">>
        end
    end,
    Server = amqp_rpc_server:start_link(Conn, pp:to_binary(Q), RPCHandler),
    erlang:display({reg_rpc_server, Q, Server}),
    NewServers = maps:put(pp:to_binary(Q), Server, Servers),
    {reply, ok, S#{servers=>NewServers}};

handle_call({unreg_rpc_server, Q}, _F, #{servers:=Servers}=S) ->
    case maps:get(pp:to_binary(Q), Servers, undefined) of
        undefined -> {reply, undefiend_queue, S};
        Server ->
            unlink(Server),
            amqp_rpc_server:stop(Server),
            NewServers = maps:remove(pp:to_binary(Q), Servers),
            {reply, ok, S#{servers=>NewServers}}
    end;

handle_call({reg_rpc_client, Q}, _F, #{connect:=Conn, clients:=Clients}=S) ->
    Client = amqp_rpc_client:start_link(Conn, pp:to_binary(Q)),
    erlang:display({reg_rpc_client, Q, Client}),
    NewClients = maps:put(pp:to_binary(Q), Client, Clients),
    {reply, ok, S#{clients=>NewClients}};

handle_call({unreg_rpc_client, Q}, _F, #{clients:=Clients}=S) ->
    case maps:get(pp:to_binary(Q), Clients, undefined) of
        undefined -> {reply, undefiend_queue, S};
        Client ->
            unlink(Client),
            amqp_rpc_client:stop(Client),
            NewClients = maps:remove(pp:to_binary(Q), Clients),
            {reply, ok, S#{clients=>NewClients}}
    end;

handle_call({rpc_call, Q, P}, _F, #{clients:=Clients}=S) when is_binary(P)->
    pp:display(Clients),
    case maps:get(pp:to_binary(Q), Clients, undefined) of
        undefined -> {reply, undefined, S};
        Client ->
            {reply, amqp_rpc_client:call(Client, P), S}
    end;
handle_call({rpc_call, Q, P}, F, S) ->
    try jiffy:encode(P) of
        P2 -> handle_call({rpc_call, Q, P2}, F, S)
    catch
        _:_ -> {reply, <<"[error, invalid_param]">>, S}
    end.


handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, #{connect:=Conn, clients:=Clients, servers:=Servers}=S) ->
    %% %% close servers
    %% lists:foreach(fun({_Q, Srv}) ->
    %%     unlink(Srv),
    %%     amqp_rpc_server:close(Srv)
    %% end, maps:to_list(Servers)),
    %% %% close clients
    %% lists:foreach(fun({_Q, C}) ->
    %%     unlink(C),
    %%     amqp_rpc_client:close(C)
    %% end, maps:to_list(Clients)),
    %% %% close connection
    amqp_connection:close(Conn),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%% private
%%%

%% Note: not all amqp_params_network fields supported.
make_network_params(Props) ->
    Pgv = fun (Key, Default) ->
                  proplists:get_value(Key, Props, Default)
          end,
    #amqp_params_network{username     = Pgv(username, <<"guest">>),
                         password     = Pgv(password, <<"guest">>),
                         virtual_host = Pgv(virtual_host, <<"/">>),
                         channel_max  = Pgv(channel_max, 0),
                         ssl_options  = Pgv(ssl_options, none),
                         host         = Pgv(host, "localhost")}.
