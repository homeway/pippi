%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(pippi_app).
-behaviour(application).

%% API.
-export([start/0, start/2]).
-export([stop/1]).

%% API.
start() -> start(none, none).
start(_Type, _Args) -> pippi_sup:start_link().
stop(_State) ->	ok.
