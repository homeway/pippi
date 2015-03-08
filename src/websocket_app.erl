%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(websocket_app).
-behaviour(application).

%% API.
-export([start/0, start/2]).
-export([stop/1]).

%% API.
start() -> start(none, none).
start(_Type, _Args) ->
	Mime = [{mimetypes,cow_mimetypes,all}],
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {file, "priv/index.html"}},
			{"/websocket", ws_handler, []},
			{"/static/[...]", cowboy_static, {dir, "priv/static"}}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
		[{env, [{dispatch, Dispatch}]}]),
	websocket_sup:start_link().

stop(_State) ->
	ok.
