-module(ets_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  ets:new(users, [named_table, set, public]),
  ets:new(sessions, [named_table, set, public]),
  {ok, []}.

handle_call({insert, Table, Key, Value}, _From, State) ->
  case ets:lookup(Table, Key) of
    [{_, _}] -> {reply, already_present, State};
    [] ->
      ets:insert(Table, {Key, Value}),
      {reply, ok, State}
  end;
handle_call({lookup, Table, Key}, _From, State) ->
  case ets:lookup(Table, Key) of
    [] -> {reply, nil, State};
    [{_, Value}] -> {reply, Value, State}
  end;
handle_call({delete, Table, User}, _From, State) ->
  case ets:lookup(Table, User) of
    [{User, _}] ->
      ets:delete(Table, User),
      {reply, ok, State};
    [] -> {reply, not_present, State}
  end.

handle_cast(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

