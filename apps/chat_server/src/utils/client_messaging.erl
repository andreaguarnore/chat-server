-module(client_messaging).

-export([send/3, broadcast/3]).

% send a message to a client socket
send(Socket, Sender, Msg) ->
  Formatted = io_lib:format("[~s] ~s\n", [Sender, Msg]),
  gen_tcp:send(Socket, Formatted).

% broadcast a message to a list of client sockets
broadcast(Sockets, Sender, Msg) ->
  Formatted = io_lib:format("[~s] ~s\n", [Sender, Msg]),
  broadcast(Sockets, Formatted).

broadcast([], _Msg) -> ok;
broadcast([Socket | Sockets], Msg) ->
  gen_tcp:send(Socket, Msg),
  broadcast(Sockets, Msg).

