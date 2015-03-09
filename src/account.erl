-module(account).
-behaviour(gen_fsm).
-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1, login/2, logout/2, locked/2]).
-export([handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-define(SERVER, ?MODULE).

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, logout, #{token=>undefined, users=>Users}}.

logout({login, From, User, Pass}, State) ->
  case maps:get(Account, all(), undefined) =:= Pass of
    true ->
      From ! [login, ok]);
    false ->
      From ! [login, failed])
  end.

    {next_state, login, State, 3000};
logout(lock, State) ->
    {next_state, locked, State, 3000}.

login(logout, State) ->
    {next_state, logout, State};
login({message, Msg}, State) ->
    erlang:display(Msg),
    {next_state, login, State, 3000};
login(timeout, State) ->
    {next_state, logout, State}.

locked(unlock, State) ->
    {next_state, logout, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) -> {next_state, StateName, State}.
terminate(_Reason, _StateName, _State) ->  ok.
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

