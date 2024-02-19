-module(client).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  {ok, Port} = application:get_env(port),
  io:format("connecting to port ~B~n", [Port]),
  case gen_tcp:connect("localhost", Port, [binary,
                                           {packet, 0},
                                           {active, false}]) of
    {ok, Socket} ->
      spawn(fun() -> handler(Socket) end),
      io:format("(type /help for help)~n", []),
      read_input(Socket),
      {ok, Socket};
    {error, Reason} ->
      io:format("failed to connect. closing the client~n", []),
      {error, Reason}
  end.

stop(Socket) ->
  gen_tcp:close(Socket).

read_input(Socket) ->
  case io:get_line("~ ") of
    "\n" -> read_input(Socket);
    "/help\n" ->
      io:format("basic commands" ++
                "\n  /help                 show this help" ++
                "\n  /quit                 quit the client" ++
                "\n  <message>             send a message" ++
                "\n  /pm <user> <message>  send a private message" ++
                "\n\nuser commands"
                "\n  /whoami               print the user name" ++
                "\n  /login <username>     login to the server" ++
                "\n  /logout               logout from the server" ++
                "\n\nroom commands" ++
                "\n  /room create <name>   create a public room" ++
                "\n  /room createp <name> <members..>" ++
                "\n                        create a private room with" ++
                "\n                        with the given members" ++
                "\n  /room delete <name>   delete a room" ++
                "\n  /room list            list all rooms" ++
                "\n  /room join <name>     join a room" ++
                "\n  /room leave           leave the current room" ++
                "~n"),
      read_input(Socket);
    "/quit\n" ->
      ok;
    Input ->
      gen_tcp:send(Socket, Input),
      read_input(Socket)
  end.

handler(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Bin} ->
      Msg = string:trim(binary_to_list(Bin)),
      io:format("\r~s~n~~ ", [Msg]),
      handler(Socket);
    {error, _} ->
      ok
  end.

