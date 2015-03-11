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
      <<"send_sms">> -> force_login(Cmd, Pid, fun send_sms/2);
      <<"sms_records">> -> force_login(Cmd, Pid, fun sms_records/2);
      <<"send_multi_sms">> -> force_login(Cmd, Pid, fun send_multi_sms/2);
      <<"resend_multi_sms">> -> force_login(Cmd, Pid, fun resend_multi_sms/2)
    end,
    % erlang:display([Type|Respond]),
    Pid ! {client, jiffy:encode([Type|Respond])}
  end),
  {reply, {text, jiffy:encode([processing, Type])}, Req, State};

websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
	erlang:start_timer(1000, self(), jiffy:encode([heart, ok])),
	{reply, {text, Msg}, Req, State};
websocket_info({client, Msg}, Req, State) ->
  {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
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

update_password(#{<<"token">> := Token, <<"password">> := Pass}) ->
  {ok, AccountName} = res_account:token_info(Token),
  {atomic, ok} = res_account:update_pass(AccountName, Pass),
  [ok].

logout(#{<<"token">> := Token}) ->
  res_account:delete_token(Token),
  [ok].

check_token(#{<<"token">> := Token}) ->
  case res_account:token_info(Token) of
    {ok, _} -> [ok];
    _ -> [erorr, not_login]
  end.

force_login(#{<<"token">> := Token}=Cmd, Pid, Fun) ->
  case res_account:token_info(Token) of
    {ok, _} -> Fun(Pid, Cmd);
    _ -> [erorr, not_login]
  end.

send_sms(_, #{<<"to">> := To,  <<"content">> := Content}) ->
  {Code, Res} = sms:send(To, Content),[Code, Res].

send_multi_sms(Pid, Item0) ->
  Item = send_multi(Item0), % sync to create a sms record
  {ok, Key} = res_sms_records:insert(Item),
  #{<<"code">> := Code, <<"respond">> := Respond} = Item,
  update_sms_status(Pid, Key), % async to update sms record for gate status
  [Code, Respond].

resend_multi_sms(Pid, #{<<"key">> := Key, <<"token">> := Token}) ->
  [{_, _, Item0}|_] = res_sms_records:get(Key),
  Item = send_multi(Item0#{<<"token">> => Token}),
  res_sms_records:update(Key, Item),
  update_sms_status(Pid, Key), % async to update sms record for gate status
  #{<<"code">> := Code, <<"respond">> := Respond} = Item,
  [Code, Respond].

sms_records(_, #{<<"token">> := _Token}) ->
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

update_sms_status(Pid, Key) ->
  spawn(fun() ->
    [{_, _, Item0}|_] = res_sms_records:get(Key),
    {Code, Status} = case maps:get(<<"code">>, Item0, undefined) of
      ok ->
        %% if code is ok, then respond is batchId
        case sms:status(maps:get(<<"respond">>, Item0)) of
          {ok, _} -> {ok, sent};
          _ -> {error, not_send}
        end;
      _ -> {error, not_submit}
    end,
    Item1 = Item0#{status => Status},
    res_sms_records:update(Key, Item1),
    Pid ! {client, jiffy:encode([sms_gate_state_update, Code, Status])}
  end).

