-module(ddb_utils).

-export([delete_item/2, get_client/0, get_item/2, put_item/2, scan/1]).

delete_item(Table, {HashKey, RangeKey}) ->
  Client = ddb_utils:get_client(),
  {TableName, {HashName, HashType}, {RangeName, RangeType}} = get_table(Table),
  Input =
    #{<<"Key">> =>
        #{HashName => #{HashType => list_to_binary(HashKey)},
          RangeName => #{RangeType => list_to_binary(RangeKey)}},
      <<"TableName">> => TableName},
  aws_dynamodb:delete_item(Client, Input);
delete_item(Table, HashKey) ->
  Client = ddb_utils:get_client(),
  {TableName, {HashName, HashType}} = get_table(Table),
  Input = #{<<"Key">> => #{HashName => #{HashType => list_to_binary(HashKey)}},
            <<"TableName">> => TableName},
  aws_dynamodb:delete_item(Client, Input).

get_client() ->
  [{_Key, Client}] = ets:lookup(aws, client),
  Client.

get_table(Table) ->
  [{_Table, TableSpec}] = ets:lookup(table_specs, Table),
  TableSpec.

get_item(Table, {HashKey, RangeKey}) ->
  Client = ddb_utils:get_client(),
  {TableName, {HashName, HashType}, {RangeName, RangeType}} = get_table(Table),
  Input =
    #{<<"Key">> =>
        #{HashName => #{HashType => list_to_binary(HashKey)},
          RangeName => #{RangeType => list_to_binary(RangeKey)}},
      <<"TableName">> => TableName},
  aws_dynamodb:get_item(Client, Input);
get_item(Table, HashKey) ->
  Client = ddb_utils:get_client(),
  {TableName, {HashName, HashType}} = get_table(Table),
  Input = #{<<"Key">> => #{HashName => #{HashType => list_to_binary(HashKey)}},
            <<"TableName">> => TableName},
  aws_dynamodb:get_item(Client, Input).

put_item(Table, Item) ->
  Client = ddb_utils:get_client(),
  {TableName, _} = get_table(Table),
  Input = #{<<"TableName">> => TableName, <<"Item">> => Item},
  aws_dynamodb:put_item(Client, Input).

scan(Table) ->
  Client = ddb_utils:get_client(),
  {TableName, _} = get_table(Table),
  Input = #{<<"TableName">> => TableName},
  {ok, #{<<"Items">> := Items}, _} = aws_dynamodb:scan(Client, Input),
  Items.


