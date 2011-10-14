-module(rets_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

env(Property, Default) ->
  case application:get_env(rets, Property) of
    {ok, Found} -> Found;
              _ -> Default
  end.

init([]) ->
  Hostname = env(redis_host, "localhost"),
  Port = env(redis_port, 6379),
  Block = env(block_during_sync, true),
  Timeout = env(initial_sync_timeout, 10 * 60 * 1000),
  NSync = nsync(Hostname, Port, Block, Timeout),
  create_ets_table(),

  Processes = [NSync],
  Strategy = {one_for_one, 10, 10},

  {ok,
   {Strategy, lists:flatten(Processes)}}.

nsync(Hostname, PortNumber, BlockYesNo, SyncTimeout) ->
  Callback = {callback, {rets_nsync_callback, do, []}},
  Host = {host, Hostname},
  Port = {port, PortNumber},
  Block = {block, BlockYesNo},
  Timeout = {timeout, SyncTimeout},
  Opts = [Callback, Block, Timeout, Host, Port, Block, Timeout],
  {nsync, {nsync, start_link, [Opts]},
   permanent, 5000, worker, [nsync]}.

create_ets_table() ->
  ets:new(rets_nsync_callback:table(),
          [named_table, public, set, {read_concurrency, true}]).
