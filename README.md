rets: unidirectional redis to erlang replication with callbacks
===============================================================

What is it?
-----------
rets replicates a redis server to an ets table with an optional callback
for each add/remove operation.

`rets` uses `nsync` to act as a complete redis replication target.

Note: we can only replicate against redis 2.2 servers so far.  redis 2.4
moved to a more efficient (and more complicated protocol) we
aren't handling yet.


Why is this special?
--------------------
Friends don't let friends use mnesia.

Logical flow:

1. You start the `rets` application.
2. `rets` connects to your redis server using `nsync`
3. `nsync` issues a redis `SYNC` command
4. redis asynchronously writes its entire dataset to disk as an RDB file
5. redis sends the data dump to `nsync`
6. `nsync` issues a `rets` callback for each received key/value.
7. `rets` runs the user-provided callback then optionally updates the
rets ets table
8. When the complete RDB dump is loaded, `rets` begins receiving lives updates
from your redis server.
9. Future operations on the connected redis server are replicated to ets in
real-time using the same mechanism redis servers use to replicate updates
between primary and secondary servers.



Usage
-----
### Constraints
Only a very tiny subset of redis commands are handled by `rets`.  `rets` is not
a redis clone in Erlang, so it doesn't support keeping up with every redis
operation.  The best use case for `rets` is having *simple* values you want
to keep in sync across many servers (hashes and strings, mainly).

### Currently Implemented Command Handlers
`rets` currently has support for replication of 
`hmset`, `hset`, `set`, and `del`
redis operations.  Any other operations passed from redis to `rets`
are ignored.

Note: the initial redis sync can load all redis data types because the inital
sync simply receives key-value pairs.  Only post-sync operations are limited
by the implemented `rets` command set.

If you need support for more operations, just add them to the {cmd, ...} handler
in `rets_nsync_callback.erl` and let us know so we can integrate your changes.


### Configuration
`rets` needs to know a few things to get your ets replication started:
redis host, redis port, optional function to run after each update, and
the ets table where data gets stored.

You can set your config using `application:set_env/3` or by specifying
settings in a VM-level `.config` file.  See files in the `test/` directory
for examples of using `set_env`.

rets environment variables:

   * `redis_host` default localhost
   * `redis_port` default 6379
   * `update_notify_function` no default
   * `ets_table` default rets


### Callback Function
Your callback function allows `rets` to act on incoming keys and optionally
deny writing of a key into the default ets table being synchronized.

Callbacks look like:

    callback({add, Type, Key, Value}) ->
      ok;
    callback({add, hash_set, Key, HashKey, HashValue}) ->
      ok;
    callback({del, Key}) ->
      ok.

Where `Type` is an atom in `hash`, `string`, `zset`, `listset`, `hash_set`.

`Type` to `Value` mappings:

   * `hash`: a `dict`
   * `string`: a binary
   * `zset`: list of tuples `[{Score1, Val1}, {Score2, Val2}, ...]`
   * `listset`: list of elements as binaries (we don't differentiate between
lists and sets).
   * `hash_set`: a `hset` or `hmset` operation happened and here is a new or updated hash key/value pair.

If your callback returns the atom `skip`, then `rets` will *not* perform
ets actions (insert/delete).

So, you can use your callback in three ways:

   * Run your own processing on redis keys and values as they arrive then let `rest` store the redis data in ets
   * Run your own processing and store nothing in ets by returning `skip`
   * Use the callback as a filter to decide what to `skip` versus store.


### Running
Make sure your rets environment is setup to define your redis connection,
callback, and ets table name before starting the application.  Starting rets
begins the redis sync process.

    application:start(rets).


Building
--------
Dependencies:
        rebar get-deps

Build:
        rebar compile


Testing
-------
Automated:

First, start a redis 2.2 server on localhost port 6992
(or change the host and port in the test module).
Then, uncomment the er dependency in rebar.conf,
run rebar get-deps, run rebar compile, then run:

        rebar eunit skip_deps=true

Todo
----

   * Better error messages (if you see a REDIS0002 error, you are trying to sync
against a redis 2.4 server which wont work.  use a redis 2.2 server.)
   * More direct command support
     * Much of it could be extracted from deleted parts of `nsync` at
https://github.com/JacobVorreuter/nsync/commit/8c2618bf85f1f25be4bced9cacc0e60472191b3a


Thanks
------
I learned the use-redis-as-synced-configuration-management-server
from JacobVorreuter.  This project is a thin wrapper around his
`nsync` project.
