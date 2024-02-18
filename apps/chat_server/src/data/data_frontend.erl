-module(data_frontend).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-define(BACKEND, ets).

% define handlers' modules and `init` wrt the defined backend
% (note: unfortunately i see no better way than defining the handlers manually,
%        as the only alternative would be to manipulate the AST to concatenate
%        the atoms defining the backend and handlers' macros)
-if(?BACKEND == ets).
-define(USER_HANDLER, ets_user_handler).
-define(ROOM_HANDLER, ets_room_handler).
-define(MESSAGING_HANDLER, ets_messaging_handler).
init([]) ->
  ets:new(state, [named_table, set, public]),
  ets:new(sessions, [named_table, set, public]), % port: user
  ets:new(rooms, [named_table, ordered_set, public]), % room name: room
  ets:insert(state, {usernames, []}), % list of user(names) logged in
  {ok, []}.
%-elif(?BACKEND == ddb).
-else.
% unexpected backend: fail compilation
-endif.

% user commands
handle_call({whoami, Args}, _From, State) ->
  {reply, ?USER_HANDLER:whoami(Args), State};
handle_call({login, Args}, _From, State) ->
  {reply, ?USER_HANDLER:login(Args), State};
handle_call({logout, Args}, _From, State) ->
  {reply, ?USER_HANDLER:logout(Args), State};

% room commands
handle_call({room_create, Args}, _From, State) ->
  {reply, ?ROOM_HANDLER:create(Args), State};
handle_call({room_createp, Args}, _From, State) ->
  {reply, ?ROOM_HANDLER:createp(Args), State};
handle_call({room_delete, Args}, _From, State) ->
  {reply, ?ROOM_HANDLER:delete(Args), State};
handle_call({room_list, Args}, _From, State) ->
  {reply, ?ROOM_HANDLER:list(Args), State};
handle_call({room_join, Args}, _From, State) ->
  {reply, ?ROOM_HANDLER:join(Args), State};
handle_call({room_leave, Args}, _From, State) ->
  {reply, ?ROOM_HANDLER:leave(Args), State};

% messaging commands
handle_call({room_message, Args}, _From, State) ->
  {reply, ?MESSAGING_HANDLER:room_message(Args), State};
handle_call({pm, Args}, _From, State) ->
  {reply, ?MESSAGING_HANDLER:private_message(Args), State}.

% unused callbacks
handle_cast(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

