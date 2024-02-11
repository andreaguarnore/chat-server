-module(messaging_handler).

-export([room_message/1]).

-include("records.hrl").

room_message({Socket, Msg}) ->
  case user_handler:whoami(Socket) of
    {ok, #user{name=UserName, room=RoomName}} when RoomName /= nil ->
      % send message to participants
      [{_RoomName, Room}] = ets:lookup(rooms, RoomName), % assume it exists
      RoomSockets = [RoomSocket || {RoomSocket, _User} <- Room#room.participants],
      Func = fun(RoomSocket) -> Socket /= RoomSocket end,
      client_messaging:broadcast(lists:filter(Func, RoomSockets), UserName, Msg),

      % show message to sender
      client_messaging:send(Socket, "you", Msg),
      ok;
    {ok, #user{name=_UserName, room=nil}} -> {error, not_in_a_room};
    {error, not_logged_in} -> {error, not_logged_in}
  end.

