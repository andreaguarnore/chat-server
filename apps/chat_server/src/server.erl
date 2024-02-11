-module(server).

-behaviour(application).

-export([start/2, stop/1]).

-include_lib("shared_include/constants.hrl").

start(_StartType, _StartArgs) ->
  {ok, LSocket} = gen_tcp:listen(?PORT, [binary,
                                         {packet, 0},
                                         {reuseaddr, true},
                                         {active, true}]),
  spawn(fun() -> accept_client(LSocket) end),
  ets_server:start_link(),
  {ok, LSocket}.

stop(LSocket) ->
  gen_tcp:close(LSocket).

accept_client(LSocket) ->
  {ok, Socket} = gen_tcp:accept(LSocket),
  io:format("[~p] accepted new socket~n", [Socket]),
  spawn(fun() -> accept_client(LSocket) end),
  handle_client(Socket).

handle_client(Socket) ->
  receive
    {tcp, Socket, Bin} ->
      UserInput = string:trim(binary:bin_to_list(Bin)),
      io:format("[~p] received `~s`~n", [Socket, UserInput]),
      Handler = case hd(UserInput) of
        $/ -> fun handle_command/2;
        _ -> fun handle_message/2
      end,
      case Handler(Socket, UserInput) of
        ok -> handle_client(Socket);
        {ok, Msg} ->
          client_messaging:send(Socket, "server", Msg),
          io:format("[~p] ok: replying `~s`~n", [Socket, Msg]),
          handle_client(Socket);
        {error, Msg} ->
          client_messaging:send(Socket, "server", Msg),
          io:format("[~p] error: replying `~s`~n", [Socket, Msg]),
          handle_client(Socket)
      end;
    {tcp_closed, Socket} ->
      _ = user_handler:logout(Socket),
      io:format("[~p] socket closed~n", [Socket])
  end.

call(Command, Args) ->
  gen_server:call(ets_server, {Command, Args}).

handle_command(Socket, UserInput) ->
  Command = string:tokens(UserInput, " "),
  case Command of

    % user commands
    ["/whoami"] ->
      case call(whoami, Socket) of
        {ok, {UserName, _}} -> {ok, io_lib:format("~s", [UserName])};
        {error, not_logged_in} -> {error, "you are not logged in!"}
      end;
    ["/login", UserName] ->
      case call(login, {Socket, UserName}) of
        ok -> {ok, io_lib:format("hello, ~s!", [UserName])};
        {error, name_taken} -> {error, io_lib:format("~s is already logged in", [UserName])};
        {error, already_logged_in} -> {error, "you are already logged in!"}
      end;
    ["/logout"] ->
      case call(logout, Socket) of
        ok -> {ok, io_lib:format("bye!", [])};
        {error, not_logged_in} -> {error, "you are not logged in!"}
      end;

    % room commands
    ["/room", "create", RoomName] ->
      case call(room_create, {Socket, RoomName}) of
        ok -> ok;
        {error, not_logged_in} -> {error, "you are not logged in!"};
        {error, name_taken} -> {error, io_lib:format("room `~s` already exists", [RoomName])}
      end;
    ["/room", "delete", RoomName] ->
      case call(room_delete, {Socket, RoomName}) of
        ok -> {ok, io_lib:format("deleted room `~s`", [RoomName])};
        {error, unauthorized} -> {error, io_lib:format("you are not the owner of room `~s`", [RoomName])};
        {error, not_found} -> {error, io_lib:format("room `~s` was not found", [RoomName])};
        {error, not_logged_in} -> {error, "you are logged in!"}
      end;
    ["/room", "list"] ->
      case call(room_list, Socket) of
        {ok, RoomsInfo} ->
          case RoomsInfo of
            [] -> {ok, "there are no rooms"};
            _ -> {ok, "rooms:\n  " ++ lists:join("\n  ", RoomsInfo)}
          end;
        {error, not_logged_in} -> {error, "you are not logged in!"}
      end;
    ["/room", "join", RoomName] ->
      case call(room_join, {Socket, RoomName}) of
        ok -> {ok, io_lib:format("joined room `~s`", [RoomName])};
        {error, not_found} -> {error, io_lib:format("room `~s` was not found", [RoomName])};
        {error, in_another_room} -> {error, "you have to leave this room first"};
        {error, not_logged_in} -> {error, "you are not logged in!"}
      end;
    ["/room", "leave"] ->
      case call(room_leave, Socket) of
        {ok, _RoomName} -> ok;
        {error, not_in_a_room} -> {error, "you are not in a room!"};
        {error, not_logged_in} -> {error, "you are not logged in!"}
      end;

    % unknown command
    _ -> {ok, "unknown command"}

  end.

handle_message(Socket, Msg) ->
  case call(room_message, {Socket, Msg}) of
    ok -> ok;
    {error, not_in_a_room} -> {error, "you are not in a room!"};
    {error, not_logged_in} -> {error, "you are not logged in!"}
  end.


