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
      FormattedReply = io_lib:format("[server] ~s", [Reply]),
      BinReply = binary:list_to_bin(FormattedReply),
      gen_tcp:send(Socket, BinReply),
      handle_client(Socket);
    {tcp_closed, Socket} ->
      _ = user_handler:logout(Socket),
      io:format("[~p] socket closed~n", [Socket])
  end.

call(Command, Args) ->
  gen_server:call(ets_server, {Command, Args}).

handle_user_input(UserInput, Socket) ->
  Command = string:tokens(UserInput, " "),
  case Command of
    ["whoami"] ->
      case call(whoami, Socket) of
        nil -> "you are not logged in!";
        User -> io_lib:format("~s", [User])
      end;
    ["login", User] ->
      case call(login, {Socket, User}) of
        ok -> io_lib:format("hello, ~s!", [User]);
        {error, name_taken} ->
          io_lib:format("~s is already logged in", [User]);
        {error, already_logged_in} ->
          "you are already logged in!"
      end;
    ["logout"] ->
      case call(logout, Socket) of
        ok -> io_lib:format("bye!", []);
        error -> "you are not logged in!"
      end;
    _ ->
      "unknown command"
  end.

