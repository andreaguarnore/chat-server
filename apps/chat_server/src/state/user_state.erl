-module(user_state).

-export([whoami/1, login/2, logout/1]).

call(Args) ->
  gen_server:call(ets_server, Args).

whoami(Socket) ->
  call({lookup, sessions, Socket}).

login(User, Socket) ->
  case call({lookup, sessions, Socket}) of
    nil ->
      case call({insert, users, User, Socket}) of
        ok ->
          ok = call({insert, sessions, Socket, User}),
          ok;
        already_present -> already_logged_in
      end;
    _ -> session_already_active
  end.

logout(Socket) ->
  case call({lookup, sessions, Socket}) of
    nil -> not_logged_in;
    User ->
      ok = call({delete, users, User}),
      ok = call({delete, sessions, Socket}),
      ok
  end.

