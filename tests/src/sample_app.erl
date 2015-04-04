-module(sample_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() -> start(none, none).
start(_Type, _Args) ->
  % Mime = [{mimetypes,cow_mimetypes,all}],
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/ws", pippi_websocket, []}
    ]}
  ]),
  {ok, _} = cowboy:start_http(
    http,
    100,
    [{port, 8964}],
    [{env, [{dispatch, Dispatch}]}]
  ),
  erlang:display(cowboy_started),
  sample_sup:start_link().

stop(_State) ->
  ok.
