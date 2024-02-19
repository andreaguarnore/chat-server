-module(ddb_user_handler).

-export([whoami/1, login/1, logout/1]).

-include("records.hrl").

whoami(Socket) ->
  case ets:lookup(sessions, Socket) of
    [{_Socket, User}] -> {ok, User};
    [] -> {error, not_logged_in}
  end.

login({Socket, UserName}) ->
  case whoami(Socket) of
    {error, not_logged_in} -> % no user in current session with this socket
      case ddb_utils:get_item(users, UserName) of
        {ok, User, _} when map_size(User) == 0 -> % non-existing user
          Item = #{<<"Name">> => #{<<"S">> => list_to_binary(UserName)}},
          ddb_utils:put_item(users, Item),
          ets:insert(sessions, {Socket, #user{name=UserName}}),
          ok;
        {ok, _, _} -> % user already logged in sometime prior
          Func = fun({_Socket, User}) -> User#user.name == UserName end,
          case ets_utils:first(Func, sessions) of
            {error, not_found} -> % no one in the session is logged in as this user
              ets:insert(sessions, {Socket, #user{name=UserName}}),
              ok;
            {ok, _} -> {error, name_taken}
          end
      end;
    {ok, _} -> {error, already_logged_in}
  end.

logout(Socket) ->
  case whoami(Socket) of
    {ok, #user{name=_UserName}} ->
      _ = ddb_room_handler:leave(Socket), % leave current room, if any
      ets:delete(sessions, Socket),
      ok;
    {error, not_logged_in} -> {error, not_logged_in}
  end.

