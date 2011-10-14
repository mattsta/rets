-module(rets_nsync_callback).

-compile(export_all).

%%%----------------------------------------------------------------------
%%% nsync callback
%%%----------------------------------------------------------------------
% cmd tuples are from live streaming redis events
do({cmd, Cmd, [Key|Value]}) ->
  case Cmd of
     "hset" -> hset(Key, Value);
    "hmset" -> hmset(Key, Value);
      "set" -> set(Key, Value);
      "del" -> del(Key);
          _ -> io:format("~nReceived unhandled command: {~p, ~p, ~p}~n",
                         [Cmd, Key, Value])
  end;

% load tuples are from the initial redis RDB dump/sync process
do({load, Key, Value}) when is_tuple(Value) ->
  load_hash(Key, Value);

do({load, Key, Value}) when is_binary(Value) ->
  load_string(Key, Value);

do({load, Key, Value}) when is_list(Value) andalso is_tuple(hd(Value)) ->
  % zset lists are [{Score, Value}, ...]
  load_zset(Key, Value);

do({load, Key, Value}) when is_list(Value) ->
  load_list_or_set(Key, Value);

do({error, closed}) ->
  io:format("Redis connection lost.  No new updates will be processed.");

do({load, eof}) ->
  error_logger:info_msg("Done with initial redis sync."),
  ok;

do(_) ->
  ok.

%%%----------------------------------------------------------------------
%%% load from redis dump during initial sync
%%%----------------------------------------------------------------------
load_hash(Key, Dict) ->
  store_hash(Key, Dict).

load_string(Key, String) when is_binary(String) ->
  store_string(Key, String).

load_zset(Key, ZSet) ->
  store_zset(Key, ZSet).

% nsync represents lists and sets the same way, so we don't know which is which
load_list_or_set(Key, List) ->
  store_list_or_set(Key, List).

%%%----------------------------------------------------------------------
%%% commands streamed from redis
%%%----------------------------------------------------------------------
hset(Key, Value) ->
  update_hash(Key, Value).

hmset(Key, Values) ->
  update_hash(Key, Values).

set(Key, Value) ->
  store_string(Key, Value).

% TODO: Check what hdel generates
del(Key) ->
  ets_del(Key).

%%%----------------------------------------------------------------------
%%% common manipulators
%%%----------------------------------------------------------------------
% The format of manipulators is:
%  * top level function for each manipulation type
%  * call a common ets modification function
%    * the ets modification is aborted if the notify callback returns 'skip'
%  The ets modification functions need to be common since load operations *and*
%  set operations need the same code path.

store_hash(Key, Dict) ->
  ets_add(hash, Key, Dict).

update_hash(_, []) -> ok;
update_hash(Key, [HashKey, HashVal | HashValPairs]) ->
  ets_hash_update(Key, HashKey, HashVal),
  update_hash(Key, HashValPairs).

store_string(Key, [String]) when is_binary(String) ->
  ets_add(string, Key, String);
store_string(Key, String) when is_binary(String) ->
  ets_add(string, Key, String).

store_zset(Key, ZSet) ->
  ets_add(zset, Key, ZSet).

store_list_or_set(Key, List) ->
  ets_add(listset, Key, List).

%%%----------------------------------------------------------------------
%%% data changers
%%%----------------------------------------------------------------------
ets_add(Type, Key, Value) ->
  case notify({add, Type, Key, Value}) of
    skip -> ok;
       _ -> ets:insert(table(), {Key, Value})
  end.

ets_hash_update(Key, HashKey, HashValue) ->
  case notify({add, hash_set, Key, HashKey, HashValue}) of
    skip -> ok;
       _ -> Dict = case ets:lookup(table(), Key) of
                     [{Key, DictA}] -> DictA;
                                 [] -> dict:new()
                   end,
            ets:insert(table(), {Key, dict:store(HashKey, HashValue, Dict)})
  end.

ets_del(Key) ->
  case notify({del, Key}) of
    skip -> ok;
       _ -> ets:delete(table(), Key)
  end.

table() ->
  case application:get_env(rets, ets_table) of
    {ok, TableName} -> TableName;
                  _ -> rets
  end.

%%%----------------------------------------------------------------------
%%% external notifications
%%%----------------------------------------------------------------------
notify(NotifyMsg) ->
  case application:get_env(rets, update_notify_function) of
    {ok, NotifyFun} when is_function(NotifyFun) -> NotifyFun(NotifyMsg);
    {ok, {M, F}} -> M:F(NotifyMsg);
               _ -> ok
  end.
