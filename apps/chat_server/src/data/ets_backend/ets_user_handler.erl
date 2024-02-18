-module(ets_user_handler).

-export([whoami/1, login/1, logout/1]).

-include("records.hrl").

whoami(Socket) ->
  case ets:lookup(sessions, Socket) of
    [{_Socket, User}] -> {ok, User};
    [] -> {error, not_logged_in}
  end.

login({Socket, UserName}) ->
  case whoami(Socket) of
    {error, not_logged_in} ->
      [{_Socket, UserNames}] = ets:lookup(state, usernames),
      case lists:member(UserName, UserNames) of
        false ->
          ets:insert(sessions, {Socket, #user{name=UserName}}),
          ets:insert(state, {usernames, [UserName | UserNames]}),
          ok;
        true ->
          {error, name_taken}
      end;
    {ok, _} -> {error, already_logged_in}
  end.

logout(Socket) ->
  case whoami(Socket) of
    {ok, #user{name=UserName}} ->
      [{_Socket, UserNames}] = ets:lookup(state, usernames),
      case lists:member(UserName, UserNames) of
        true ->
          _ = ets_room_handler:leave(Socket), % leave current room, if any
          ets:delete(sessions, Socket),
          ets:insert(state, {usernames, lists:delete(UserName, UserNames)}),
          ok;
        false ->
          {error, already_logged_in}
      end;
    {error, not_logged_in} -> {error, not_logged_in}
  end.

