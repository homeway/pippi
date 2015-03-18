-module(auto_run).
-export([start/0]).

start() ->
  Apps = [crypto, cowlib, ranch, cowboy, websocket, mnesia, sync],
  [application:start(A) || A <- Apps].

