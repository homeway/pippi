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
websocket_handle({text, Msg}, Req, #{account:=A}=State) ->
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
        [<<"raw_sync">>, M, F, A] ->
            apply(pp:to_atom(M), pp:to_atom(F), pp:to_atom(A));
        _ -> [error, unknown_action, Cmds]
    end,
    erlang:display({"client text req: ", [Cmds]}),
    {reply, {text, jiffy:encode(pp:to_list(R))}, Req, State};

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({client, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

