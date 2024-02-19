-define(BACKEND, ddb).

% define handlers' modules wrt the defined backend
% (note: unfortunately i see no better way than defining the handlers manually,
%        as the only alternative would be to manipulate the AST to concatenate
%        the atoms defining the backend and handlers' macros)
-if(?BACKEND == ets).
-define(USER_HANDLER, ets_user_handler).
-define(ROOM_HANDLER, ets_room_handler).
-define(MESSAGING_HANDLER, messaging_utils).

-elif(?BACKEND == ddb).
-define(USER_HANDLER, ddb_user_handler).
-define(ROOM_HANDLER, ddb_room_handler).
-define(MESSAGING_HANDLER, messaging_utils).

-endif.

