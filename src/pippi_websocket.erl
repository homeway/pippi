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
websocket_handle({text, Msg}, Req, #{account:=Ac, methods:=Methods}=State) ->
    Cmds = jiffy:decode(Msg, [return_maps]),
    R = case Cmds of
        [<<"login">>, [User, Pass]] ->
            case Ac:login(User, Pass) of
                ok -> self() ! update_methods, ok;
                Error -> Error
            end;
        [<<"logout">>] ->
            R1 = Ac:logout(),
            self() ! update_methods, R1;
        [<<"status">>] ->
            Ac:status();
        [<<"methods">>] ->
            maps:get(methods, State, []);
        [M, F] ->
            call(M, F, [], Methods);
        [M, F, A] ->
            call(M, F, A, Methods);
        _ ->
            erlang:display(Cmds),
            pp:display(Cmds),
            [error, no_this_action, Cmds]
    end,
    pp:display({"req : ", Cmds}),
    pp:display({"resp: ", R}),
    {reply, {text, jiffy:encode(pp:confirm_json(R))}, Req, State};

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
