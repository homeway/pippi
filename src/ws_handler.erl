-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
	erlang:start_timer(1000, self(), jiffy:encode([system, init])),
	{ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
  io:format("~ts", [Msg]),
  [Type, Cmd] = jiffy:decode(Msg, [return_maps]),
  TextResult = case Type of
    <<"login">> -> login(Cmd);
    <<"send_sms">> -> send_sms(Cmd);
    <<"send_multi_sms">> -> send_multi_sms(Cmd);
    <<"status_sms">> -> status_sms(Cmd)
  end,
  {reply, {text, TextResult}, Req, State};

websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
	erlang:start_timer(1000, self(), jiffy:encode([heart, ok])),
	{reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.

%% api for ws client
%% --------------------------
login(#{<<"account">> := Account, <<"password">> := Pass}) ->
  Users = #{
    <<"gonghao">> => <<"123">>,
    <<"xiaoliao">> => <<"456">>
  },
  case maps:get(Account, Users, undefined) =:= Pass of
    true ->
      jiffy:encode([login, ok]);
    false ->
      jiffy:encode([login, failed])
  end.

send_sms(#{<<"sms">> := <<"single">>, <<"to">> := To, <<"content">> := Content}) ->
  case sms:send(To, Content) of
    {ok, BatchId}   ->
      jiffy:encode([send_sms, ok]);
    {error, Reason} ->
      jiffy:encode([send_sms, failed])
  end.

send_multi_sms(#{<<"sms">> := <<"multi">>, <<"to">> := To, <<"content">> := Content}) ->
  case sms:send_multi(To, Content) of
    {ok, BatchId}   ->
      jiffy:encode([send_sms, ok]);
    {error, Reason} ->
      jiffy:encode([send_sms, failed])
  end.

status_sms(#{<<"sms">> := <<"status">>, <<"batchid">> := BatchId}) ->
  case sms:status(BatchId) of
    {ok, _}         ->
      jiffy:encode([status_sms, ok]);
    {error, Reason} ->
      jiffy:encode([status_sms, failed])
  end.
