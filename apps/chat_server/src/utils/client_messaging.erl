-module(client_messaging).

-export([send/3, broadcast/3]).

send(Socket, Sender, Msg) ->
  Formatted = io_lib:format("[~s] ~s", [Sender, Msg]),
  gen_tcp:send(Socket, Formatted).

broadcast(Sockets, Sender, Msg) ->
  Formatted = io_lib:format("[~s] ~s", [Sender, Msg]),
  broadcast(Sockets, Formatted).

broadcast([], _Msg) -> ok;
broadcast([Socket | Sockets], Msg) ->
  gen_tcp:send(Socket, Msg),
  broadcast(Sockets, Msg).

