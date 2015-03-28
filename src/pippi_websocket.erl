%% -*- mode:nitrogen -*-
-module(pippi_websocket).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

%% connect to pp_account
websocket_init(_TransportName, Req, _Opts) ->
    Ac = pp_account:start_link(),
    Methods = Ac:methods(),
    {ok, Req, #{account=>Ac, methods=>Methods}}.

%% text command
%%
%% {text, json(login, A)}
%% {text, json(logout)}
%% {text, json(raw, M, F, A)}
%% {text, json(amqp, M, F, A)}
command([<<"login">>, [User, Pass]], #{account:=Ac}) ->
    case Ac:login(User, Pass) of
        ok -> self() ! update_methods, ok;
        Error -> Error
    end;
command([<<"logout">>], #{account:=Ac}) ->
    R1 = Ac:logout(),
    self() ! update_methods, R1;
command([<<"status">>], #{account:=Ac}) ->
    Ac:status();
command([<<"methods">>], #{methods:=Methods}) ->
    Methods;
command([M, F], #{methods:=Methods}) ->
    call(M, F, [], Methods);
command([M, F, A], #{methods:=Methods}) ->
    call(M, F, A, Methods);
command(Cmd, _) ->
    erlang:display(Cmd),
    pp:display(Cmd),
    [error, no_this_action, Cmd].

websocket_handle({text, Msg}, Req, State) ->
    R = case jiffy:decode(Msg, [return_maps]) of
        [<<"call">>, Seq, Cmd] ->
            [Seq, pp:confirm_json(command(Cmd, State))];
        Cmd ->
            pp:confirm_json(command(Cmd, State))
    end,
    pp:display({"req : ", Cmd}),
    pp:display({"resp: ", R}),
    {reply, {text, jiffy:encode(R)}, Req, State};

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info(offline, Req, State) ->
    {reply, {text, jiffy:encode(offline)}, Req, State};

websocket_info(update_methods, Req, #{account:=Ac}=State) ->
    {ok, Req, State#{methods=>Ac:methods()}};

websocket_info({client, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

call([<<"nosqlite">>, M0], F0, A, Methods) ->
    M = pp:to_atom(M0),
    F = pp:to_atom(F0),
    case pp:allow([{nosqlite, M}, F, A], Methods) of
        true -> apply({nosqlite, M}, F, A);
        _ -> {error, deny}
    end;
call(M0, F0, A, Methods) ->
    M = pp:to_atom(M0),
    F = pp:to_atom(F0),
    case pp:allow([M, F, A], Methods) of
        true -> apply(M, F, A);
        _ -> {error, deny}
    end.
