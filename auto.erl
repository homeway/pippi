-module(auto).
-export([start/0]).

start() ->
  [application:start(A) || A <- [crypto, cowlib, ranch, cowboy, websocket, mnesia, sync]].
