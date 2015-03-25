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
    A = pp_account:start_link(),
    Methods = A:methods(),
    {ok, Req, #{account=>A, methods=>Methods}}.

%% text command
%%
%% {text, json(login, A)}
%% {text, json(logout)}
%% {text, json(raw, M, F, A)}
%% {text, json(amqp, M, F, A)}
websocket_handle({text, Msg}, Req, #{account:=A, methods:=Methods}=State) ->
    Cmds = jiffy:decode(Msg, [return_maps]),
    R = case Cmds of
        [<<"login">>, [User, Pass]] ->
            case A:login(User, Pass) of
                ok -> self() ! update_methods, ok;
                Error -> Error
            end;
        [<<"logout">>] ->
            R1 = A:logout(),
            self() ! update_methods, R1;
        [<<"status">>] ->
            A:status();
        [<<"methods">>] ->
            maps:get(methods, State, []);
        [<<"call">>, [<<"nosqlite">>, M], F] ->
            call([nosqlite, pp:to_atom(M)], pp:to_atom(F), [], Methods, Cmds);
        [<<"call">>, M, F] ->
            call(pp:to_atom(M), pp:to_atom(F), [], Methods, Cmds);
        [<<"call">>, [<<"nosqlite">>, M], F, A] ->
            call([nosqlite, pp:to_atom(M)], pp:to_atom(F), A, Methods, Cmds);
        [<<"call">>, M, F, A] ->
            call(pp:to_atom(M), pp:to_atom(F), A, Methods, Cmds);
        _ ->
            [error, no_this_action, Cmds]
    end,
    pp:display({"req : ", Cmds}),
    pp:display({"resp: ", R}),
    {reply, {text, jiffy:encode(pp:to_list(R))}, Req, State};

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({client, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

call([nosqlite, M], F, A, Methods, Cmds) ->
    case pp:allow([[nosqlite, M], F, A], Methods) of
        true -> pp:apply([nosqlite, M], F, A);
        _ -> [error, deny, Cmds]
    end;
call(M, F, A, Methods, Cmds) ->
    case pp:allow([M, F, A], Methods) of
        true -> pp:apply(M, F, A);
        _ -> [error, deny, Cmds]
    end.
