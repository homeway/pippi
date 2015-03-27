%% -*- mode: nitrogen -*-
-module(pp_account).
-behaviour(gen_fsm).
-export([start/0, start/2, stop/1, start_link/0, start_link/2]).
-export([account_tuple/1, login/3, logout/1, status/1, methods/1]).

%% gen_fsm callbacks
-export([init/1, offline/2, offline/3, online/2, online/3]).
-export([handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(TIMEOUT, 1000).

%% api
start() -> start(3000, 3000).
start(OnTimeout, OffTimeout) ->
    {?MODULE, Pid} = start_link(OnTimeout, OffTimeout),
    unlink(Pid),
    {?MODULE, Pid}.

start_link() -> start_link(3000, 3000).
start_link(OnTimeout, OffTimeout) ->
    {ok, Pid} = gen_fsm:start_link(?MODULE, [#{
        online_timeout => OnTimeout,
        offline_timeout => OffTimeout
    }], []),
    {?MODULE, Pid}.

stop({?MODULE, Pid}) ->
    gen_fsm:send_all_state_event(Pid, stop).

account_tuple(Pid) ->
    {?MODULE, Pid}.

login(User, Pass, {?MODULE, Pid}) ->
    Auth = #{
        user => pp:to_binary(User),
        pass => pp:to_binary(Pass)
    },
    gen_fsm:sync_send_event(Pid, {login, Auth}, ?TIMEOUT).

logout({?MODULE, Pid}) ->
    gen_fsm:sync_send_event(Pid, logout, ?TIMEOUT).

status({?MODULE, Pid}) ->
    gen_fsm:sync_send_all_state_event(Pid, status, ?TIMEOUT).

methods({?MODULE, Pid}) ->
    gen_fsm:sync_send_event(Pid, methods, ?TIMEOUT).

%% gen_fsm callbacks
init([#{offline_timeout:=OfflineTimeout}=Conf]) ->
    {ok, offline, Conf, OfflineTimeout}.

%％ login when offline
offline({login, #{user := User, pass := Pass}}, _From, State) ->
    Tab = nosqlite:table(?MODULE),
    case Tab:find(user, eq, User) of
        [_, #{pass:=Pass}, _] ->
            {reply, ok, online, State#{user=> User}, maps:get(online_timeout, State)};
        _ ->
            {reply, {error, bad_user_or_password}, offline, State, maps:get(offline_timeout, State)}
    end;

%% return methods not yet login
offline(methods, _From, State) ->
    Tab = nosqlite:table(pp_role),
    [_, #{methods:=Methods}, _] = Tab:get(everyone),
    {reply, Methods, offline, State, maps:get(offline_timeout, State)};

%% no action
offline(_, _, State) ->
    {reply, {error, offline, no_this_action}, offline, State, maps:get(offline_timeout, State)}.


%% offline session to exit
offline(timeout, State) ->
    {stop, normal, State};

offline(_Event, State) ->
    {next_state, online, State, maps:get(offline_timeout, State)}.

%% to logout
online(logout, _From, State) ->
    {reply, ok, offline, State, maps:get(offline_timeout, State)};

%% return methods after login
online(methods, _From, #{user:=User}=State) ->
    TabUser = nosqlite:table(?MODULE),
    TabRole = nosqlite:table(pp_role),
    [_, #{roles:=Roles}, _] = TabUser:find(user, eq, User),
    Methods = lists:foldl(fun(Role, Acc) ->
        [_, #{methods:=Methods1}, _] = TabRole:get(Role),
        Acc ++ Methods1
    end, [], [everyone|Roles]),
    {reply, Methods, online, State, maps:get(offline_timeout, State)};

%% no action
online(_, _, State) ->
    {reply, {error, online, no_this_action}, online, State, maps:get(online_timeout, State)}.

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

