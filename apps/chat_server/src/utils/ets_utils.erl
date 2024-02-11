-module(ets_utils).

-export([set_fold/3]).

% fold on an ets set
set_fold(Func, Acc, Table) ->
  FirstKey = ets:first(Table),
  set_fold(Func, Acc, Table, FirstKey).

set_fold(Func, Acc, Table, CurrentKey) ->
  case ets:lookup(Table, CurrentKey) of
    [] -> Acc;
    [{_, Value}] ->
      NextKey = ets:next(Table, CurrentKey),
      set_fold(Func, Func({CurrentKey, Value}, Acc), Table, NextKey)
  end.

