-module(data_frontend).

-behaviour(gen_server).

-include("backend_macro.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% define `init` wrt the defined backend
-if(?BACKEND == ets).
init([]) ->
  ets:new(state, [named_table, set, public]),
  ets:new(sessions, [named_table, set, public]), % port: user record
  ets:new(rooms, [named_table, ordered_set, public]), % room name: room record
  ets:insert(state, {usernames, []}), % list of user(names) logged in
  {ok, []}.

-elif(?BACKEND == ddb).
init([]) ->
  ets:new(aws, [named_table, set, public]),
  ets:new(sessions, [named_table, set, public]),
  ets:new(table_specs, [named_table, set, public]),
  {ok, Key} = application:get_env(aws_key),
  {ok, Secret} = application:get_env(aws_secret),
  {ok, Port} = application:get_env(ddb_port),
  {ok, Hostname} = application:get_env(ddb_hostname),
  Client = aws_client:make_local_client(list_to_binary(Key),
                                        list_to_binary(Secret),
                                        list_to_binary(Port),
                                        list_to_binary(Hostname)),
  % keep table specs to facilitate getting, deleting, and scanning items;
  % for items to be added the handler is still required to pass in the specs
  ets:insert(aws, {client, Client}),
  ets:insert(table_specs, {users, {<<"Users">>, {<<"Name">>, <<"S">>}}}),
  ets:insert(table_specs, {rooms, {<<"Rooms">>, {<<"Name">>, <<"S">>}}}),
  ets:insert(table_specs, {messages, {<<"Messages">>, {<<"Room">>, <<"S">>},
                                                      {<<"Timestamp">>, <<"S">>}}}),
  {ok, []}.

-else.
% unexpected backend: fail compilation by not defining `init`
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

