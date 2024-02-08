-module(chat_server_app).

-behavior(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
  io:fwrite("hello, world\n"),
  {ok, self()}.

stop(_State) -> ok.

