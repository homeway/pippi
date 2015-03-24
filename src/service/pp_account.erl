%% -*- mode: nitrogen -*-
-module(pp_account).
-behaviour(gen_fsm).
-export([start/0, start/2, stop/1]).
-export([account_tuple/1, login/3, status/1]).

%% gen_fsm callbacks
-export([init/1, offline/2, offline/3, online/2, online/3]).
-export([handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(TIMEOUT, 1000).

%% api
start() -> start(3000, 3000).
start(OnlineTimeout, OfflineTimeout) ->
    {ok, Pid} = gen_fsm:start(?MODULE, [#{
        online_timeout => OnlineTimeout,
        offline_timeout => OfflineTimeout,
        subscribes => #{}
    }], []),
    {?MODULE, Pid}.

stop({?MODULE, Pid}) ->
    gen_fsm:send_all_state_event(Pid, stop).

account_tuple(Pid) ->
    {?MODULE, Pid}.

login(User, Pass, {?MODULE, AccountPid}) ->
    Auth = #{
        user => pp:to_binary(User),
        pass => pp:to_binary(Pass)
    },
    gen_fsm:sync_send_event(AccountPid, {login, Auth}, ?TIMEOUT).

status({?MODULE, Pid}) ->
    gen_fsm:sync_send_all_state_event(Pid, status, ?TIMEOUT).

%% gen_fsm callbacks
init([#{offline_timeout:=OfflineTimeout}=Conf]) ->
    {ok, offline, Conf, OfflineTimeout}.

%％ login when offline
offline({login, #{user := <<"adi">>, pass := <<"123">>}}, _From, State) ->
    {reply, ok, online, State#{user=> <<"adi">>}, maps:get(online_timeout, State)};

%% login when offline
offline({login, _Auth}, _From, State) ->
    {reply, {error, bad_user_or_password}, offline, State, maps:get(offline_timeout, State)}.

%% offline session to exit
offline(timeout, State) ->
    {stop, normal, State};

offline(_Event, State) ->
    {next_state, online, State, maps:get(offline_timeout, State)}.

%% to logout
online(logout, _From, State) ->
    {reply, ok, offline, State}.

%% online session timeout
online(timeout, State) ->
    {next_state, offline, State, maps:get(offline_timeout, State)};

online(_Event, State) ->
    {next_state, online, State, maps:get(online_timeout, State)}.

%% stop action
handle_event(stop, _StateName, State) ->
    {stop, normal, State};

%% status
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(status, _From, StateName, State) ->
    {reply, {StateName, State}, StateName, State};

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, no_this_action, StateName, State}.

handle_info(_Info, StateName, State) -> {next_state, StateName, State}.
terminate(_Reason, _StateName, _State) -> ok.
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

