-module(ddb_room_handler).

-export([create/1, createp/1, delete/1, list/1, join/1, leave/1]).

-include("records.hrl").

create({Socket, RoomName}) ->
  case ddb_user_handler:whoami(Socket) of
    {ok, #user{name=UserName}} ->
      case ddb_utils:get_item(rooms, RoomName) of
        {ok, Room, _} when map_size(Room) == 0 -> % room name is available
          Item = #{<<"Name">> => #{<<"S">> => list_to_binary(RoomName)},
                   <<"Owner">> => #{<<"S">> => list_to_binary(UserName)},
                   <<"Type">> => #{<<"S">> => <<"public">>},
                   <<"Members">> => #{<<"S">> => <<"">>}},
          ddb_utils:put_item(rooms, Item),
          ok;
        {ok, _, _} -> {error, name_taken}
      end;
    {error, not_logged_in} -> {error, not_logged_in}
  end.

createp({_Socket, _RoomName, _Members}) ->
  ok.

delete({Socket, RoomName}) ->
  case ddb_user_handler:whoami(Socket) of
    {ok, #user{name=UserName}} ->
      case ddb_utils:get_item(rooms, RoomName) of
        {ok, #{<<"Item">> := #{<<"Owner">> := #{<<"S">> := RoomOwner}}}, _} ->
          case binary_to_list(RoomOwner) of
            UserName -> delete_room(RoomName);
            _ -> {error, unauthorised}
          end;
        {ok, _, _} -> {error, not_found}
      end;
    {error, not_logged_in} -> {error, not_logged_in}
  end.

list(Socket) ->
  case ddb_user_handler:whoami(Socket) of
    {ok, #user{name=UserName}} ->
      Rooms = ddb_utils:scan(rooms),
      Func = fun(Room, Acc) ->
               #{<<"Name">> := #{<<"S">> := RoomNameBin},
                 <<"Owner">> := #{<<"S">> := RoomOwnerBin},
                 <<"Type">> :=#{<<"S">> := RoomTypeBin}} = Room,
               RoomName = binary_to_list(RoomNameBin),
               RoomOwner = binary_to_list(RoomOwnerBin),
               RoomType = binary_to_list(RoomTypeBin),
               case RoomType of
                 "public" ->
                   [room_to_string(UserName, {RoomName, RoomOwner, RoomType}) | Acc];
                 _ -> % private room
                   Acc % todo
               end
             end,
      {ok, lists:foldl(Func, [], Rooms)};
    {error, not_logged_in} -> {error, not_logged_in}
  end.

join({Socket, RoomName}) ->
  case ddb_user_handler:whoami(Socket) of
    {ok, #user{name=UserName, room=nil}} -> % the user is not in a room
      case ddb_utils:get_item(rooms, RoomName) of
        {ok, #{<<"Item">> := Room}, _} -> % room exists
          #{<<"Type">> := #{<<"S">> := RoomType},
            <<"Members">> := #{<<"S">> := MembersAsStr}} = Room,
          RoomMembers = string:tokens(binary_to_list(MembersAsStr), "|"),
          case is_user_authorised(UserName, RoomType, RoomMembers) of
            true -> add_user_to_room(Socket, UserName, RoomName);
            _ -> {error, not_found}
          end;
        {ok, _, _} -> {error, not_found}
      end;
    {ok, #user{name=_UserName, room=_CurrentRoomName}} -> % the user is in another room
                                                        % (or potentially the same!)
      ok;
    {error, not_logged_in} -> {error, not_logged_in}
  end.

leave(Socket) ->
  case ddb_user_handler:whoami(Socket) of
    {ok, #user{name=UserName, room=RoomName}} when RoomName /= nil ->
      remove_user_from_room(Socket, UserName, RoomName);
    {ok, _} -> {error, not_in_a_room};
    {error, not_logged_in} -> {error, not_logged_in}
  end.

% pretty print a room
room_to_string(CurrentUserName, {RoomName, RoomOwner, RoomType}) ->
  Owner = case RoomOwner of
            CurrentUserName -> "[owner]";
            _ -> ""
          end,
  FormatArgs = [RoomName, RoomType, Owner],
  io_lib:format("~s (~s) ~s", FormatArgs).

delete_room(RoomName) ->
  % broadcast a message to all participants in the room
  FilterFunc = fun({_Socket, User}) -> User#user.room == RoomName end,
  Participants = ets_utils:filter(FilterFunc, sessions),
  ParticipantsSockets = [ParticipantSocket || {ParticipantSocket, _Participant} <- Participants],
  Msg = io_lib:format("room `~s` has been deleted, you have been kicked", [RoomName]),
  client_messaging:broadcast(ParticipantsSockets, "server", Msg),

  % update the sessions of all participants
  Func = fun({Socket, UserName}) ->
           ets:insert(sessions, {Socket, #user{name=UserName, room=nil}})
         end,
  lists:map(Func, Participants),

  % delete the room
  ddb_utils:delete_item(rooms, RoomName),
  ok.

is_user_authorised(UserName, RoomType, RoomMembers) ->
  case RoomType of
    <<"public">> -> true;
    _ -> lists:member(UserName, RoomMembers) % private room
  end.

add_user_to_room(UserSocket, UserName, RoomName) ->
  ets:insert(sessions, {UserSocket, #user{name=UserName, room=RoomName}}),
  ddb_messaging_handler:send_message_to_room(UserSocket,
                                             UserName,
                                             "joined the room",
                                             RoomName),
  ok.

remove_user_from_room(UserSocket, UserName, RoomName) ->
  ddb_messaging_handler:send_message_to_room(UserSocket,
                                             UserName,
                                             "left the room",
                                             RoomName),
  ets:insert(sessions, {UserSocket, #user{name=UserName, room=nil}}),
  ok.
