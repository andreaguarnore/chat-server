-module(ets_room_handler).

-export([create/1, createp/1, delete/1, list/1, join/1, leave/1]).

-include("records.hrl").

create({Socket, RoomName}) ->
  case ets_user_handler:whoami(Socket) of
    {ok, #user{name=UserName}} ->
      case ets:lookup(rooms, RoomName) of
        [] ->
          Room = #room{owner=UserName},
          ets:insert(rooms, {RoomName, Room}),
          ok;
        % in theory, if there is a private room by the same name, this will
        % return `name_taken`, even if the user should not be able to see
        % such room
        [{_RoomName, _Room}] -> {error, name_taken}
      end;
    {error, not_logged_in} -> {error, not_logged_in}
  end.

createp({Socket, RoomName, Members}) ->
  case ets_user_handler:whoami(Socket) of
    {ok, #user{name=UserName}} ->
      % take the sockets of all (unique) members (including the creator)
      AllMembers = sets:from_list([UserName | Members]),
      Func = fun({_CandidateSocket, CandidateUser}) ->
               sets:is_element(CandidateUser#user.name, AllMembers)
             end,
      AvailableMembers = [{MemberSocket, Member#user.name} ||
          {MemberSocket, Member} <- ets_utils:filter(Func, sessions)],
      % add all (existing) given users as members to the new room
      case ets:lookup(rooms, RoomName) of
        [] ->
          Room = #room{owner=UserName, type=private, members=AvailableMembers},
          ets:insert(rooms, {RoomName, Room}),
          ok;
        [{_RoomName, _Room}] -> {error, name_taken}
      end;
    {error, not_logged_in} -> {error, not_logged_in}
  end.

delete({Socket, RoomName}) ->
  case ets_user_handler:whoami(Socket) of
    {ok, #user{name=UserName}} ->
      case ets:lookup(rooms, RoomName) of
        [{_, Room}] when Room#room.owner == UserName -> delete_room(RoomName, Room);
        [{_, _}] -> {error, unauthorised};
        [] -> {error, not_found}
      end;
    {error, not_logged_in} -> {error, not_logged_in}
  end.

list(Socket) ->
  case ets_user_handler:whoami(Socket) of
    {ok, #user{name=UserName}} ->
      % fold on the rooms to retrieve a list of strings where
      % each element contains information about a room; skip
      % private rooms where the user is not a member
      Func = fun({RoomName, Room}, Acc) ->
               if
                 Room#room.type == public -> % public room
                   [room_to_string(UserName, {RoomName, Room}) | Acc];
                 true -> % private room -> check if the current user is a member
                   case lists:member({Socket, UserName}, Room#room.members) of
                     true -> [room_to_string(UserName, {RoomName, Room}) | Acc];
                     false -> Acc
                   end
               end
             end,
      {ok, ets:foldl(Func, [], rooms)};
    {error, not_logged_in} -> {error, not_logged_in}
  end.

join({Socket, RoomName}) ->
  case ets_user_handler:whoami(Socket) of
    {ok, #user{name=UserName, room=nil}} -> % the user is not in a room
      case ets:lookup(rooms, RoomName) of
        [{_RoomName, Room}] ->
          case is_user_authorised(Socket, UserName, Room) of
            true -> add_user_to_room(Socket, UserName, RoomName, Room);
            _ -> {error, not_found}
          end;
        [] -> {error, not_found}
      end;
    {ok, #user{name=UserName, room=CurrentRoomName}} -> % the user is in another room
                                                        % (or potentially the same!)
      case ets:lookup(rooms, RoomName) of
        [{_NewRoomName, NewRoom}] ->
          case is_user_authorised(Socket, UserName, NewRoom) of
            true -> % move the user from one room to the other
              [{_RoomName, CurrentRoom}] = ets:lookup(rooms, CurrentRoomName),
              remove_user_from_room(Socket, UserName, CurrentRoomName, CurrentRoom),
              add_user_to_room(Socket, UserName, RoomName, NewRoom);
            _ -> {error, not_found}
          end;
        [] -> {error, not_found}
      end;
    {error, not_logged_in} -> {error, not_logged_in}
  end.

leave(Socket) ->
  case ets_user_handler:whoami(Socket) of
    {ok, #user{name=UserName, room=RoomName}} when RoomName /= nil ->
      [{_RoomName, Room}] = ets:lookup(rooms, RoomName),
      remove_user_from_room(Socket, UserName, RoomName, Room);
    {ok, _} -> {error, not_in_a_room};
    {error, not_logged_in} -> {error, not_logged_in}
  end.

% pretty print a room
room_to_string(CurrentUserName, {RoomName, Room}) ->
  Owner = case Room#room.owner of
            CurrentUserName -> "[owner]";
            _ -> ""
          end,
  FormatArgs = [RoomName, Room#room.type,
                length(Room#room.participants), Owner],
  io_lib:format("~s (~s, ~B participants) ~s", FormatArgs).

% assumes that the room exists
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
  ets:delete(rooms, RoomName),
  ok.

% returns whether the user is authorised to join a room or not
is_user_authorised(UserSocket, UserName, Room) ->
  case Room#room.type of
    private -> lists:member({UserSocket, UserName}, Room#room.members);
    _ -> true % public room
  end.

% assumes that the user exists and
% that they are not in another room;
% additionally, it broadcasts a message to all participants
add_user_to_room(UserSocket, UserName, RoomName, Room) ->
  Participants = [{UserSocket, UserName} | Room#room.participants],
  ets:insert(rooms, {RoomName, Room#room{participants=Participants}}),
  ets:insert(sessions, {UserSocket, #user{name=UserName, room=RoomName}}),
  messaging_utils:send_message_to_room(UserSocket,
                                       UserName,
                                       "joined the room",
                                       Room),
  ok.

% assumes that the user exists and,
% if also the room exists, then the user is in it;
% additionally, it broadcasts a message to all participants
remove_user_from_room(UserSocket, UserName, RoomName, Room) ->
  messaging_utils:send_message_to_room(UserSocket,
                                       UserName,
                                       "left the room",
                                       Room),
  Participants = lists:delete({UserSocket, UserName}, Room#room.participants),
  ets:insert(rooms, {RoomName, Room#room{participants=Participants}}),
  ets:insert(sessions, {UserSocket, #user{name=UserName, room=nil}}),
  ok.

