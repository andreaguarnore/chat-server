-module(ets_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  ets:new(state, [named_table, set, public]),
  ets:new(sessions, [named_table, set, public]), % port: user
  ets:new(rooms, [named_table, ordered_set, public]), % room name: room
  ets:insert(state, {usernames, []}), % list of user(names) logged in
  {ok, []}.

% user commands
handle_call({whoami, Args}, _From, State) ->
  {reply, user_handler:whoami(Args), State};
handle_call({login, Args}, _From, State) ->
  {reply, user_handler:login(Args), State};
handle_call({logout, Args}, _From, State) ->
  {reply, user_handler:logout(Args), State};

% room commands
handle_call({room_create, Args}, _From, State) ->
  {reply, room_handler:create(Args), State};
handle_call({room_delete, Args}, _From, State) ->
  {reply, room_handler:delete(Args), State};
handle_call({room_list, Args}, _From, State) ->
  {reply, room_handler:list(Args), State};
handle_call({room_join, Args}, _From, State) ->
  {reply, room_handler:join(Args), State};
handle_call({room_leave, Args}, _From, State) ->
  {reply, room_handler:leave(Args), State};

% messaging commands
handle_call({room_message, Args}, _From, State) ->
  {reply, messaging_handler:room_message(Args), State};
handle_call({pm, Args}, _From, State) ->
  {reply, messaging_handler:private_message(Args), State}.

% unused callbacks
handle_cast(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

