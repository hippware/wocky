-module(wocky_watcher).

-compile({parse_transform, cut}).
-compile({parse_transform, do}).

-export([
         register/2,
         unregister/2,

         watch/3,
         unwatch/3,
         unwatch_all/2,

         watchers/2
        ]).

-ifdef(TEST).
-export([key/1]).
-endif.

-define(NODE_CLEANUP_PRIORITY, 80).
-define(UNSET_PRESENCE_PRIORITY, 90).

-type class() :: atom().

-spec register(class(), ejabberd:server()) -> ok.
register(Class, Host) ->
    ejabberd_hooks:add(node_cleanup, global,
                       node_cleanup_hook(Class, _),
                       ?NODE_CLEANUP_PRIORITY),
    ejabberd_hooks:add(unset_presence_hook, Host,
                       unset_presence_hook(Class, _, _, _, _),
                       ?UNSET_PRESENCE_PRIORITY),
    ok.

-spec unregister(class(), ejabberd:server()) -> ok.
unregister(Class, Host) ->
    ejabberd_hooks:delete(unset_presence_hook, Host,
                          unset_presence_hook(Class, _, _, _, _),
                          ?UNSET_PRESENCE_PRIORITY),
    ok.

-spec watch(class(), ejabberd:jid(), ejabberd:jid()) -> ok.
watch(Class, User, Object) ->
    ejabberd_redis:cmd(["SADD", key(Class, Object), val(User, node())]),
    ok.

-spec unwatch(class(), ejabberd:jid(), ejabberd:jid()) -> ok.
unwatch(Class, User, Object) ->
    ejabberd_redis:cmd(["SREM", key(Class, Object), val(User, node())]),
    ok.

-spec unwatch_all(class(), ejabberd:jid()) -> ok.
unwatch_all(Class, UserJID) ->
    cleanup(Class, delete_by_jid(UserJID, _, _)).

-spec watchers(class(), ejabberd:jid()) -> [ejabberd:jid()].
watchers(Class, Object) ->
    Vals = ejabberd_redis:cmd(["SMEMBERS", key(Class, Object)]),
    lists:map(fun(V) -> element(1, val(V)) end, Vals).

%%%===================================================================
%%% MIM hook handlers
%%%===================================================================

-spec node_cleanup_hook(class(), node()) -> ok.
node_cleanup_hook(Class, Node) ->
    cleanup(Class, delete_by_node(Node, _, _)).

-spec unset_presence_hook(class(), ejabberd:luser(),
                          ejabberd:lserver(), ejabberd:lresource(),
                          binary()) -> ok.
unset_presence_hook(Class, User, Server, Resource, _Status) ->
    unwatch_all(Class, jid:make(User, Server, Resource)).

%%%===================================================================
%%% Helpers
%%%===================================================================
%%%
cleanup(Class, CleanupFun) ->
    Keys = ejabberd_redis:cmd(["KEYS", key(Class)]),
    lists:foreach(
      fun(K) ->
              Vals = ejabberd_redis:cmd(["SMEMBERS", K]),
              lists:foreach(CleanupFun(K, _), Vals)
      end, Keys).

key(Class) ->
    ["watch:", atom_to_list(Class), ":*"].
key(Class, Object) ->
    ["watch:", atom_to_list(Class), ":", jid:to_binary(Object)].

val(User, Node) ->
    term_to_binary({User, Node}).
val(Bin) ->
    binary_to_term(Bin).

delete_by_jid(UserJID, K, V) ->
    {ValJID, _} = val(V),
    case jid:are_equal(UserJID, ValJID) of
        true -> ejabberd_redis:cmd(["SREM", K, V]);
        false -> ok
    end.

delete_by_node(Node, K, V) ->
    {_, NodeStr} = val(V),
    case Node of
        NodeStr -> ejabberd_redis:cmd(["SREM", K, V]);
        _ -> ok
    end.
