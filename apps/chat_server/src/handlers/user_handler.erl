-module(user_handler).

-export([whoami/1, login/1, logout/1]).

whoami(Socket) ->
  case ets:lookup(sessions, Socket) of
    [] -> nil;
    [{_, User}] -> User
  end.

login({Socket, User}) ->
  case whoami(Socket) of
    nil ->
      [{_, Users}] = ets:lookup(state, users),
      case lists:member(User, Users) of
        true ->
          {error, name_taken};
        false ->
          ets:insert(sessions, {Socket, User}),
          ets:insert(state, {users, [User | Users]}),
          ok
      end;
    _ ->
      {error, already_logged_in}
  end.

logout(Socket) ->
  case whoami(Socket) of
    nil ->
      error;
    User ->
      [{_, Users}] = ets:lookup(state, users),
      case lists:member(User, Users) of
        true ->
          ets:delete(sessions, Socket),
          ets:insert(state, {users, lists:delete(User, Users)}),
          ok;
        false ->
          error
      end
  end.

