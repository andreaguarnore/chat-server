-module(ets_utils).

-export([first/2, filter/2]).

% returns {ok, <elem>},       where <elem> is the first element on which the
%                             predicate returns `true`, or
%         {error, not_found}, otherwise
first(Func, Table) ->
  FirstKey = ets:first(Table),
  first(Func, Table, FirstKey).

first(Func, Table, CurrentKey) ->
  case ets:lookup(Table, CurrentKey) of
    [] -> {error, not_found};
    [KeyValuePair] ->
      case Func(KeyValuePair) of
        true -> {ok, KeyValuePair};
        false ->
          NextKey = ets:next(Table, CurrentKey),
          first(Func, Table, NextKey)
      end
  end.

% filter on an ets set
filter(Func, Table) ->
  FirstKey = ets:first(Table),
  filter(Func, Table, FirstKey, []).

filter(Func, Table, CurrentKey, Acc) ->
  case ets:lookup(Table, CurrentKey) of
    [] -> Acc;
    [CurrentElem] ->
      NextKey = ets:next(Table, CurrentKey),
      NewAcc = case Func(CurrentElem) of
        true -> [CurrentElem | Acc];
        false -> Acc
      end,
      filter(Func, Table, NextKey, NewAcc)
  end.

