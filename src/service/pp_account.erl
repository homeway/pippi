%% -*- mode: nitrogen -*-
-module(pp_account).
-behaviour(gen_fsm).
-export([start/0, start/2, stop/1]).
-export([account/1, login/3, notify/2]).

%% gen_fsm callbacks
-export([init/1, offline/2, online/2]).
-export([handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% api
start() -> start(3000, 3000).
start(OnlineTimeout, OfflineTimeout) ->
    {ok, Pid} = gen_fsm:start(?MODULE, [#{online_timeout=>OnlineTimeout, offline_timeout=>OfflineTimeout}], []),
    {?MODULE, Pid}.

stop({?MODULE, Pid}) ->
    gen_fsm:send_all_state_event(Pid, stop).

account(Pid) ->
    {?MODULE, Pid}.

login(User, Pass, {?MODULE, AccountPid}) ->
    Auth = #{
        user => pp:to_binary(User),
        pass => pp:to_binary(Pass)
    },
    Ref = make_ref(),
    gen_fsm:send_event(AccountPid, {login, self(), Ref, Auth}), Ref.

notify(Msg, {?MODULE, AccountPid}) ->
    Ref = make_ref(),
    gen_fsm:send_event(AccountPid, {notify, self(), make_ref(), Msg}), Ref.

%% gen_fsm callbacks
init([#{offline_timeout:=OfflineTimeout}=Conf]) ->
    {ok, offline, maps:merge(#{slots=>[]}, Conf), OfflineTimeout}.

offline({login, From, Ref, #{user := <<"adi">>, pass := <<"123">>}}, #{slots:=SlotsOld}=State) ->
    SlotsNew = [From|SlotsOld],
    % confirm
    From ! {login, Ref, ok},
    % broad cast
    {next_state, online, State#{user=> <<"adi">>, slots=> SlotsNew}, maps:get(online_timeout, State)};

offline({login, From, Ref, Auth}, State) ->
    From ! {login, Ref, {error, Auth}},
    {next_state, offline, State, maps:get(offline_timeout, State)};

%% offline session to exit
offline(timeout, State) ->
    {stop, normal, State};

%% send a notify to offline
offline({notify, From, Ref, _OfflineMsg}, State) ->
    % confirm
    From ! {notify, Ref, {ok, offline}},
    {next_state, offline, State};

offline(_Event, State) ->
    {next_state, online, State, maps:get(offline_timeout, State)}.

%% send a notify to online
online({notify, From, Ref, Msg}, #{slots:=Slots}=State) ->
    % confirm
    From ! {notify, Ref, {ok, online}},
    % broad cast
    [Pid ! {notify, Ref, Msg} || Pid <- Slots],
    {next_state, online, State, maps:get(online_timeout, State)};

%% to logout
online({logout,From, Ref}, State) ->
    From ! {logout, Ref, ok},
    {next_state, offline, State};

%% online session timeout
online(timeout, State) ->
    {next_state, offline, State, maps:get(offline_timeout, State)};

online(_Event, State) ->
    {next_state, online, State, maps:get(online_timeout, State)}.

%% stop action
handle_event(stop, _StateName, State) ->
    {stop, normal, State};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) -> {reply, ok, StateName, State}.
handle_info(_Info, StateName, State) -> {next_state, StateName, State}.
terminate(_Reason, _StateName, _State) -> ok.
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

