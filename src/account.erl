%% -*- mode: nitrogen -*-
-module(account).
-behaviour(gen_fsm).
-export([start/0]).

%% gen_fsm callbacks
-export([init/1, offline/2, online/2]).
-export([handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-define(ONLINE_TIMEOUT, 30000).
-define(OFFLINE_TIMEOUT, 30000).


start() ->
    gen_fsm:start(?MODULE, [], []).

init([]) ->
    erlang:display({init, now(), ok}),
    {ok, offline, #{slots=>[]}, ?OFFLINE_TIMEOUT}.

offline({login, From, #{<<"user">> := <<"adi">>, <<"pass">> := <<"123">>}}, #{slots:=SlotsOld}=State) ->
    SlotsNew => [From|SlotsOld],
    %% return the result for caller
    From ! {login, ok},
    erlang:display({login, now(), ok}),

    {next_state, online, State#{user=> <<"adi">>, slots=> SlotsNew}, ?ONLINE_TIMEOUT};

offline({login, From, Auth}, State) ->
    From ! {login, error, now(), Auth},
    erlang:display({login, error, Auth}),
    {next_state, offline, State, ?ONLINE_TIMEOUT};

offline(timeout, State) ->
    erlang:display({timeout, now(), to_exit}),
    {stop, normal, State};

offline(OfflineMsg, State) ->
    erlang:display({offline_message, now(), OfflineMsg}),
    {next_state, offline, State}.

online({message, Msg}, State) ->
    erlang:display(Msg),
    erlang:display(State),
    {next_state, online, State, ?ONLINE_TIMEOUT};

online(logout, State) ->
    {next_state, offline, State};

online(timeout, State) ->
    erlang:display({timeout, to_logout}),
    {next_state, offline, State, ?OFFLINE_TIMEOUT};

online(Other, State) ->
    erlang:display(Other),
    erlang:display(State),
    {next_state, online, State, ?ONLINE_TIMEOUT}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(Info, StateName, State) ->
    erlang:display({handle_info, Info}),
    {next_state, StateName, State}.

terminate(_Reason, _StateName, State) ->
    erlang:display(exit),
    erlang:display(State), ok.

code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

