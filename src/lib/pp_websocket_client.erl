%% -*- mode: nitrogen -*-
-module(pp_websocket_client).
-behaviour(websocket_client_handler).

-export([
         connect/1,
         close/1,
         send_text/2,
         init/2,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3
        ]).

connect(Path) ->
    {ok, Pid} = websocket_client:start_link(Path, ?MODULE, [self()]),
    unlink(Pid),
    {?MODULE, Pid}.

close({?MODULE, Pid}) ->
    websocket_client:cast(Pid, stop).

send_text(Msg, {?MODULE, Pid}) ->
    websocket_client:cast(Pid, {text, pp:to_binary(Msg)}).

init([Pid], _ConnState) ->
    {ok, Pid}.

websocket_handle(Msg, _ConnState, Pid=State) ->
    Pid ! Msg,
    {ok, State}.

websocket_info(stop, _ConnState, State) ->
    {stop, normal, State};

websocket_info(Cmd, _ConnState, State) ->
    {reply, {text, Cmd}, State}.

websocket_terminate({close, _Code, _Payload}, _ConnState, _State) -> ok.
