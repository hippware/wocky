-module(wocky_watcher).

-compile({parse_transform, cut}).
-compile({parse_transform, do}).

-export([
         register/2,
         unregister/2,

         watch/3,
         unwatch/3,

         watchers/2
        ]).

-define(NODE_CLEANUP_PRIORITY, 80).
-define(REMOVE_CONNECTION_PRIORITY, 90).

-record(watcher,
        {
         object :: ejabberd:simple_jid() | '_',
         jid    :: ejabberd:simple_jid() | '_',
         node   :: node() | '_'
        }).

-type class() :: atom().

-spec register(class(), ejabberd:server()) -> ok.
register(Class, Host) ->
    wocky_mnesia:initialise_shared_ram_table(
      table(Class),
      [{type, set}],
      record_info(fields, watcher)),

    ejabberd_hooks:add(node_cleanup, global,
                       node_cleanup_hook(Class, _),
                       ?NODE_CLEANUP_PRIORITY),
    ejabberd_hooks:add(sm_remove_connection_hook, Host,
                       remove_connection_hook(Class, _, _, _, _),
                       ?REMOVE_CONNECTION_PRIORITY),
    ok.

-spec unregister(class(), ejabberd:server()) -> ok.
unregister(Class, Host) ->
    ejabberd_hooks:delete(sm_remove_connection_hook, Host,
                          remove_connection_hook(Class, _, _, _, _),
                          ?REMOVE_CONNECTION_PRIORITY),
    ok.

-spec watch(class(), ejabberd:jid(), ejabberd:jid()) -> ok.
watch(Class, User, Object) ->
    mnesia:dirty_write(table(Class), make_record(User, Object)),
    ok.

-spec unwatch(class(), ejabberd:jid(), ejabberd:jid()) -> ok.
unwatch(Class, User, Object) ->
    mnesia:dirty_delete_object(table(Class), make_record(User, Object)),
    ok.

-spec watchers(class(), ejabberd:jid()) -> [ejabberd:jid()].
watchers(Class, Object) ->
    BareWatchers = mnesia:dirty_match_object(
                     table(Class),
                     #watcher{object = jid:to_lower(Object), _ = '_'}),
    lists:map(jid:make(_), BareWatchers).


%%%===================================================================
%%% MIM hook handlers
%%%===================================================================

node_cleanup_hook(Class, Node) ->
    Table = table(Class),
    ToClean = mnesia:dirty_match_object(Table, #watcher{node = Node, _ = '_'}),
    lists:foreach(mnesia:dirty_delete_object(Table, _), ToClean).

remove_connection_hook(Class, _SID, JID, _Info, _Reason) ->
    mnesia:dirty_delete_object(table(Class),
                               #watcher{object = '_',
                                        jid = jid:to_lower(JID),
                                        node = node()}).

%%%===================================================================
%%% Helpers
%%%===================================================================

table(Class) ->
    list_to_atom(atom_to_list(Class) ++ "_watcher_table").

make_record(User, Object) ->
    #watcher{jid = jid:to_lower(User),
             object = jid:to_lower(Object),
             node = node()}.
