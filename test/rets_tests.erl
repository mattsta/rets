-module(rets_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_REDIS_HOST, "localhost").
-define(TEST_REDIS_PORT, 6992).

%%%----------------------------------------------------------------------
%%% Prelude
%%%----------------------------------------------------------------------
rets_test_() ->
  {setup,
    fun setup/0,
    fun teardown/1,
    [
     {"Sync empty redis",
       fun check_ets/0},
     {"Populate redis",
       fun populate_redis/0},
     {"Verify populate synced",
       fun verify_populate/0},
     {"Restart rets with empty ets",
       fun restart_rets/0},
     {"Verify restart synced",
       fun verify_populate/0},
     {"Populate more things",
       fun populate_more_redis/0},
     {"Verify more things populated",
       fun verify_populate_more_redis/0}
    ]
  }.

%%%----------------------------------------------------------------------
%%% Tests
%%%----------------------------------------------------------------------
check_ets() ->
  ?assertEqual([], ets:tab2list(bob)).

populate_redis() ->
  er:hmset(redis, hashA, [fieldA, valA]),
  er:set(redis, stringB, valB),
  er:set(redis, stringC, valC),
  er:del(redis, stringC).

verify_populate() ->
  ?assertEqual([{<<"stringB">>, <<"valB">>}],
    ets:lookup(bob, <<"stringB">>)),
  [{FoundKey, DictA}] = ets:lookup(bob, <<"hashA">>),
  ?assertEqual(FoundKey, <<"hashA">>),
  ?assertEqual({ok, <<"valA">>}, dict:find(<<"fieldA">>, DictA)),
  ?assertEqual([], ets:lookup(bob, stringC)).

restart_rets() ->
  application:stop(rets),
  application:start(rets).

populate_more_redis() ->
  er:set(redis, stringE, valE).

verify_populate_more_redis() ->
  ?assertEqual([{<<"stringE">>, <<"valE">>}],
    ets:lookup(bob, <<"stringE">>)).

%%%----------------------------------------------------------------------
%%% Set it up, tear it down
%%%----------------------------------------------------------------------
setup() ->
  er_pool:start_link(redis, ?TEST_REDIS_HOST, ?TEST_REDIS_PORT),
  er:flushall(redis),
  application:set_env(rets, redis_host, ?TEST_REDIS_HOST),
  application:set_env(rets, redis_port, ?TEST_REDIS_PORT),
  application:set_env(rets, update_notify_function, fun callback/1),
  application:set_env(rets, ets_table, bob),
  application:start(rets).

callback(Something) ->
  error_logger:info_msg("Got callback with: ~p~n", [Something]),
  ok.

teardown(_) ->
  application:stop(rets).
