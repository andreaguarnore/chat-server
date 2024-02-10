-module(server).

-behaviour(application).

-export([start/2, stop/1]).

-include_lib("shared_include/constants.hrl").

start(_StartType, _StartArgs) ->
  {ok, LSocket} = gen_tcp:listen(?PORT, [binary,
                                         {packet, 0},
                                         {reuseaddr, true},
                                         {active, true}]),
  spawn(fun() -> accept_client(LSocket) end),
  ets_server:start_link(),
  {ok, LSocket}.

stop(LSocket) ->
  gen_tcp:close(LSocket).

accept_client(LSocket) ->
  {ok, Socket} = gen_tcp:accept(LSocket),
  io:format("[~p] accepted new socket~n", [Socket]),
  spawn(fun() -> accept_client(LSocket) end),
  handle_client(Socket).

handle_client(Socket) ->
  receive
    {tcp, Socket, Bin} ->
      UserInput = string:trim(binary:bin_to_list(Bin)),
      io:format("[~p] received `~s`~n", [Socket, UserInput]),
      Reply = handle_user_input(UserInput, Socket),
      io:format("[~p] responding `~s`~n", [Socket, Reply]),
      BinReply = binary:list_to_bin(Reply),
      gen_tcp:send(Socket, BinReply),
      handle_client(Socket);
    {tcp_closed, Socket} ->
      _ = user_handler:logout(Socket),
      io:format("[~p] socket closed~n", [Socket])
  end.

handle_user_input(UserInput, Socket) ->
  Command = string:tokens(UserInput, " "),
  case Command of
    ["whoami"] -> user_handler:whoami(Socket);
    ["login", User] -> user_handler:login(User, Socket);
    ["logout"] -> user_handler:logout(Socket);
    _ -> "[server] unknown command"
  end.

