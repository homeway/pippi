%% -*- mode: nitrogen -*-
-module(pp_account).
-behaviour(gen_fsm).
-export([start/0, start/2, stop/1, start_link/0, start_link/2]).
-export([table/1, login/3, logout/1, status/1, methods/1]).

%% gen_fsm callbacks
-export([init/1, offline/2, offline/3, online/2, online/3]).
-export([handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(CallTimeout, 1000).

%% api
start() -> start(3000, self()).
start(SessionTimeout, Slot) ->
    {ok, Pid} = gen_fsm:start(?MODULE, [#{session_timeout => SessionTimeout, slot => Slot}], []),
    {?MODULE, Pid}.

%% default timeout is 1 hour
start_link() -> start_link(36000, self()).
start_link(SessionTimeout, Slot) ->
    {ok, Pid} = gen_fsm:start_link(?MODULE, [#{
        session_timeout => SessionTimeout,
        slot => Slot
    }], []),
    {?MODULE, Pid}.

stop({?MODULE, Pid}) ->
    gen_fsm:send_all_state_event(Pid, stop).

table(Pid) ->
    {?MODULE, Pid}.

login(User, Pass, {?MODULE, Pid}) ->
    Auth = #{
        user => pp:to_binary(User),
        pass => pp:to_binary(Pass)
    },
    gen_fsm:sync_send_event(Pid, {login, Auth}, ?CallTimeout).

logout({?MODULE, Pid}) ->
    gen_fsm:sync_send_event(Pid, logout, ?CallTimeout).

status({?MODULE, Pid}) ->
    gen_fsm:sync_send_all_state_event(Pid, status, ?CallTimeout).

%% convert module and function to atom() in methods
methods({?MODULE, Pid}) ->
    Methods1 = gen_fsm:sync_send_event(Pid, methods, ?CallTimeout),
    lists:map(fun(I) ->
        case I of
            [M1, Funs1] when is_list(Funs1) ->
                Funs2 = lists:map(fun(F) ->
                    case F of
                        [Fun|Args] -> [pp:to_atom(Fun)|Args];
                        Fun -> pp:to_atom(Fun)
                    end
                end, Funs1),
                [pp:to_atom(M1), Funs2];
            [M1, Fun] ->
                [pp:to_atom(M1, Fun)];
            M1 -> pp:to_atom(M1)
        end
    end, Methods1).

%% gen_fsm callbacks
init([State]) ->
    {ok, offline, State}.

%％ login when offline
offline({login, #{user := User, pass := Pass}}, _From, #{session_timeout:=T}=State) ->
    Tab = nosqlite:table(?MODULE),
    case Tab:find(user, eq, User) of
        [_, #{<<"pass">> := Pass}, _] ->
            {reply, ok, online, State#{user=>User}, T};
        _ ->
            {reply, {error, bad_user_or_password}, offline, State}
    end;

%% return methods not yet login
offline(methods, _From, State) ->
    Tab = nosqlite:table(pp_role),
    [_, #{<<"methods">> := Methods}, _] = Tab:get(<<"everyone">>),
    {reply, Methods, offline, State};

%% no action
offline(_, _, State) ->
    {reply, {error, offline, no_this_action}, offline, State}.

%% offline session to exit
offline(timeout, State) ->
    {stop, normal, State};

offline(_Event, #{session_timeout:=T}=State) ->
    {next_state, online, State, T}.

%% to logout
online(logout, _From, #{session_timeout:=T}=State) ->
    {reply, ok, offline, State, T};

%% return methods after login
online(methods, _From, #{user:=User, session_timeout:=T}=State) ->
    TabUser = nosqlite:table(?MODULE),
    TabRole = nosqlite:table(pp_role),
    [_, #{<<"roles">> := Roles}, _] = TabUser:find(user, eq, User),
    Methods = lists:foldl(fun(Role, Acc) ->
        [_, #{<<"methods">> := Methods1}, _] = TabRole:get(Role),
        Acc ++ Methods1
    end, [], [everyone|Roles]),
    {reply, Methods, online, State, T};

%% no action
online(_, _From, #{session_timeout:=T}=State) ->
    {reply, {error, online, no_this_action}, online, State, T}.

%% online session timeout
online(timeout, #{slot:=Slot}=State) ->
    Slot ! offline,
    {next_state, offline, State};

online(_, #{session_timeout:=T}=State) ->
    {next_state, online, State, T}.


%% stop action
handle_event(stop, _StateName, State) ->
    {stop, normal, State};

%% status
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(status, _From, StateName, State) ->
    {reply, {StateName, maps:remove(slot, State)}, StateName, State};

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, no_this_action, StateName, State}.

handle_info(_Info, StateName, State) -> {next_state, StateName, State}.
terminate(_Reason, _StateName, _State) -> ok.
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

