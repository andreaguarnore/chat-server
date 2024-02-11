-module(room_handler).

-export([create/1, delete/1, list/1, join/1, leave/1]).

-include("records.hrl").

create({Socket, RoomName}) ->
  case user_handler:whoami(Socket) of
    {ok, #user{name=UserName}} ->
      case ets:lookup(rooms, RoomName) of
        [] ->
          Room = #room{owner=UserName},
          ets:insert(rooms, {RoomName, Room}),
          ok;
        [{_RoomName, _Room}] -> {error, name_taken}
      end;
    {error, not_logged_in} -> {error, not_logged_in}
  end.

delete({Socket, RoomName}) ->
  case user_handler:whoami(Socket) of
    {ok, #user{name=UserName}} ->
      case ets:lookup(rooms, RoomName) of
        [{_, Room}] when Room#room.owner == UserName ->
          delete_room(RoomName, Room);
        [{_, _}] -> {error, unauthorized};
        [] -> {error, not_found}
      end;
    {error, not_logged_in} -> {error, not_logged_in}
  end.

delete_room(RoomName, Room) ->
  % broadcast a message to all participants in the room
  RoomSockets = [Socket || {Socket, _UserName} <- Room#room.participants],
  Msg = io_lib:format("room `~s` has been deleted, you have been kicked", [RoomName]),
  client_messaging:broadcast(RoomSockets, "server", Msg),

  % update the sessions of all participants
  Func = fun({Socket, UserName}) ->
             ets:insert(sessions, {Socket, #user{name=UserName, room=nil}})
         end,
  lists:map(Func, Room#room.participants),

  % delete the room
  ets:delete(rooms, Room),
  ok.

list(Socket) ->
  case user_handler:whoami(Socket) of
    {ok, #user{name=UserName}} ->
      Func = fun(Room, Acc) -> [room_to_string(UserName, Room) | Acc] end,
      {ok, ets_utils:set_fold(Func, [], rooms)};
    {error, not_logged_in} -> {error, not_logged_in}
  end.

room_to_string(CurrentUserName, {RoomName, Room}) ->
  Owner = case Room#room.owner of
    CurrentUserName -> "[owner]";
    _ -> ""
  end,
  FormatArgs = [RoomName, Room#room.type,
                length(Room#room.participants), Owner],
  io_lib:format("~s (~s, ~B participants) ~s", FormatArgs).

join({Socket, RoomName}) ->
  case user_handler:whoami(Socket) of
    {ok, #user{name=UserName, room=nil}} ->
      case ets:lookup(rooms, RoomName) of
        [{_RoomName, Room}] ->
          Participants = [{Socket, UserName} | Room#room.participants],
          ets:insert(rooms, {RoomName, Room#room{participants=Participants}}),
          ets:insert(sessions, {Socket, #user{name=UserName, room=RoomName}}),
          ok;
        [] -> {error, not_found}
      end;
    {ok, _} -> {error, in_another_room};
    {error, not_logged_in} -> {error, not_logged_in}
  end.

leave(Socket) ->
  case user_handler:whoami(Socket) of
    {ok, #user{name=UserName, room=RoomName}} when RoomName /= nil ->
      [{_RoomName, Room}] = ets:lookup(rooms, RoomName), % assume it exists
      Participants = lists:delete({Socket, UserName}, Room#room.participants),
      ets:insert(rooms, {RoomName, Room#room{participants=Participants}}),
      ets:insert(sessions, {Socket, #user{name=UserName, room=nil}}),
      {ok, RoomName};
    {ok, _} -> {error, not_in_a_room};
    {error, not_logged_in} -> {error, not_logged_in}
  end.

