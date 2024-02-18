-module(ets_messaging_handler).

-export([room_message/1, private_message/1]).
-export([send_message_to_room/4]).

-include("records.hrl").

room_message({Socket, Msg}) ->
  case ets_user_handler:whoami(Socket) of
    {ok, #user{name=UserName, room=RoomName}} when RoomName /= nil -> % user in a room
      [{_RoomName, Room}] = ets:lookup(rooms, RoomName), % assume it exists
      send_message_to_room(Socket, UserName, Msg, Room);
    {ok, #user{name=_UserName, room=nil}} -> {error, not_in_a_room};
    {error, not_logged_in} -> {error, not_logged_in}
  end.

private_message({Socket, Receiver, Msg}) ->
  case ets_user_handler:whoami(Socket) of
    {ok, #user{name=SenderUserName}} ->
      % find receiver in sessions
      ReceiverPredicate = fun({_Socket, User}) -> User#user.name == Receiver end,
      case ets_utils:first(ReceiverPredicate, sessions) of
        {ok, {ReceiverSocket, _}} ->
          FormattedSender = io_lib:format("~s via pm", [SenderUserName]),
          client_messaging:send(ReceiverSocket, FormattedSender, Msg),
          ok;
        {error, not_found} -> {error, not_found}
      end;
    {error, not_logged_in} -> {error, not_logged_in}
  end.

% assumes that the user is in the room where the message is sent and,
% of course, that the room exists
send_message_to_room(UserSocket, UserName, Msg, Room) ->
  % send message to all participants (except the current user)
  RoomSockets = [RoomSocket || {RoomSocket, _User} <- Room#room.participants],
  FilterFunc = fun(RoomSocket) -> UserSocket /= RoomSocket end,
  FilteredSockets = lists:filter(FilterFunc, RoomSockets),
  client_messaging:broadcast(FilteredSockets, UserName, Msg),

  % show message to current user
  client_messaging:send(UserSocket, "you", Msg),
  ok.

