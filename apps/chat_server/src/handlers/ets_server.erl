-module(ets_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  ets:new(state, [named_table, set, public]),
  ets:new(sessions, [named_table, set, public]),
  ets:insert(state, {users, []}),
  {ok, []}.

% user commands
handle_call({whoami, Args}, _From, State) ->
  {reply, user_handler:whoami(Args), State};
handle_call({login, Args}, _From, State) ->
  {reply, user_handler:login(Args), State};
handle_call({logout, Args}, _From, State) ->
  {reply, user_handler:logout(Args), State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

