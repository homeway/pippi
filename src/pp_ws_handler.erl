-module(pp_ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

%% 初始化时连接到account进程
websocket_init(_TransportName, Req, _Opts) ->
  {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
  Cmds = jiffy:decode(Msg, [return_maps]),
  pp:display(Msg),
  erlang:display({"client text req: ", [Cmds]}),
  {reply, {text, jiffy:encode([processing])}, Req, State};

websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({client, Msg}, Req, State) ->
  {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.

