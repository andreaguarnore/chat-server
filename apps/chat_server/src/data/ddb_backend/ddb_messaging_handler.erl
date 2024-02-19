-module(ddb_messaging_handler).

-export([room_message/1, private_message/1]).
-export([send_message_to_room/4]).

-include("records.hrl").

room_message({_Socket, _Msg}) -> ok.

private_message({_Socket, _Receiver, _Msg}) -> ok.

send_message_to_room(UserSocket, UserName, Msg, Room) ->
  % send message to all participants (except the current user)
  FilterFunc = fun({ParticipantSocket, Participant}) ->
                 ParticipantSocket /= UserSocket andalso
                 Participant#user.room == Room
               end,
  Participants = ets_utils:filter(FilterFunc, sessions),
  ParticipantsSockets = [ParticipantSocket || {ParticipantSocket, _Participant} <- Participants],
  client_messaging:broadcast(ParticipantsSockets, UserName, Msg),

  % show message to current user
  client_messaging:send(UserSocket, "you", Msg),
  ok.

