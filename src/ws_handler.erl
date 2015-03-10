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
  io:format("~ts\n", [Msg]),
  [Type, Cmd] = jiffy:decode(Msg, [return_maps]),
  Pid = self(),
  spawn(fun() ->
    Respond = case Type of
      <<"login">> -> login(Cmd);
      <<"logout">> -> logout(Cmd);
      <<"check_token">> -> check_token(Cmd);
      <<"send_sms">> -> force_login(Cmd, fun send_sms/1);
      <<"sms_records">> -> force_login(Cmd, fun sms_records/1);
      <<"send_multi_sms">> -> force_login(Cmd, fun send_multi_sms/1);
      <<"resend_multi_sms">> -> force_login(Cmd, fun resend_multi_sms/1);
      <<"status_sms">> -> force_login(Cmd, fun status_sms/1)
    end,
    erlang:display([Type|Respond]),
    Pid ! {client, jiffy:encode([Type|Respond])}
  end),
  {reply, {text, jiffy:encode([ok, procesing, Type])}, Req, State};

websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
	erlang:start_timer(1000, self(), jiffy:encode([heart, ok])),
	{reply, {text, Msg}, Req, State};
websocket_info({client, Msg}, Req, State) ->
  erlang:display("........client"),
  {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
  erlang:display("........info"),
  erlang:display(Req),
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.

%% api for ws client
%% --------------------------
login(#{<<"account">> := Account, <<"password">> := Pass}) ->
  case res_account:check_pass(Account, Pass) of
    true ->
      Token = res_account:generate_token(Account),
      [ok, Token];
    false ->
      [error, invalid_account_or_password]
  end.

logout(#{<<"token">> := Token}) ->
  res_account:delete_token(Token),
  [ok].

check_token(#{<<"token">> := Token}) ->
  case res_account:token_info(Token) of
    {ok, _} -> [ok];
    _ -> [erorr, not_login]
  end.

force_login(#{<<"token">> := Token}=Cmd, Fun) ->
  case res_account:token_info(Token) of
    {ok, _} -> Fun(Cmd);
    _ -> [erorr, not_login]
  end.

send_sms(#{<<"to">> := To,  <<"content">> := Content}) ->
  {Code, Res} = sms:send(To, Content),[Code, Res].

send_multi_sms(Item0) ->
  Item = send_multi(Item0),
  res_sms_records:insert(Item),
  #{<<"code">> := Code, <<"respond">> := Respond} = Item,
  [Code, Respond].

resend_multi_sms(#{<<"key">> := Key}) ->
  [{_, _, Item0}|_] = res_sms_records:get(Key),
  Item = send_multi(Item0),
  res_sms_records:update(Key, Item),
  #{<<"code">> := Code, <<"respond">> := Respond} = Item,
  [Code, Respond].

status_sms(#{<<"batchid">> := BatchId}) ->
  {Code, Res} = sms:status(BatchId), [Code, Res].

sms_records(#{<<"token">> := _Token}) ->
  Res = [R#{key=>Key} || {_, Key, R} <- res_sms_records:all()],
  [ok, Res].

send_multi(Item) ->
  #{<<"token">> := Token, <<"contacts">> := Contacts, <<"content">> := Content} = Item,
  To = [Tel || #{tel := Tel} <- Contacts],
  [Code, Respond] = case sms:send_multi(To, Content) of
    {ok, BatchId} -> [ok, BatchId];
    {error, timeout} -> [error, <<"短信网关访问超时"/utf8>>];
    {error, Reason} -> [error, Reason]
  end,
  {ok, Account} = res_account:token_info(Token),
  Item#{<<"account">> => Account, <<"code">> => Code, <<"respond">> => Respond}.

