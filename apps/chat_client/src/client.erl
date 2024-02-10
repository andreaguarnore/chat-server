-module(client).

-behaviour(application).

-export([start/2, stop/1]).

-include_lib("shared_include/constants.hrl").

start(_StartType, _StartArgs) ->
  {ok, Socket} = gen_tcp:connect("localhost", ?PORT, [binary,
                                                      {packet, 0},
                                                      {active, false}]),
  spawn(fun() -> handler(Socket) end),
  io:format("(type help for help)~n", []),
  read_input(Socket),
  {ok, Socket}.

stop(Socket) ->
  gen_tcp:close(Socket).

read_input(Socket) ->
  case io:get_line("~ ") of
    "\n" -> read_input(Socket);
    "help\n" ->
      io:format("basic commands" ++
                "\n  help" ++
                "\n  quit" ++
                "\n\nuser commands"
                "\n  whoami" ++
                "\n  login <username>" ++
                "\n  logout" ++
                "~n"),
      read_input(Socket);
    "quit\n" ->
      ok;
    Input ->
      gen_tcp:send(Socket, Input),
      read_input(Socket)
  end.

handler(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Bin} ->
      Msg = string:trim(binary:bin_to_list(Bin)),
      io:format("\r  ~s~n~~ ", [Msg]),
      handler(Socket);
    {error, _} ->
      ok
  end.

