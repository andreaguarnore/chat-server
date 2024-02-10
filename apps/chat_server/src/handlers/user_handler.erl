-module(user_handler).

-export([whoami/1, login/2, logout/1]).

whoami(Socket) ->
  case user_state:whoami(Socket) of
    nil ->
      io_lib:format("[server] you are not logged in!", []);
    User ->
      io_lib:format("[server] ~s", [User])
  end.

login(User, Socket) ->
  case user_state:login(User, Socket) of
    ok ->
      io_lib:format("[server] hello, ~s!", [User]);
    already_logged_in ->
      io_lib:format("[server] ~s is already logged in", [User]);
    session_already_active ->
      io_lib:format("[server] you are already logged in", [])
  end.

logout(Socket) ->
  case user_state:logout(Socket) of
    ok ->
      io_lib:format("[server] bye!", []);
    not_logged_in ->
      io_lib:format("[server] you are not logged in!", [])
  end.

