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
-export([table/1]).
-endif.

-define(NODE_CLEANUP_PRIORITY, 80).
-define(UNSET_PRESENCE_PRIORITY, 90).

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
      [{type, bag},
       {record_name, watcher}],
      record_info(fields, watcher)),

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
    mnesia:dirty_write(table(Class), make_record(User, Object)),
    ok.

-spec unwatch(class(), ejabberd:jid(), ejabberd:jid()) -> ok.
unwatch(Class, User, Object) ->
    mnesia:dirty_delete_object(table(Class), make_record(User, Object)),
    ok.

-spec unwatch_all(class(), ejabberd:jid()) -> ok.
unwatch_all(Class, UserJID) ->
    cleanup(Class,
            #watcher{object = '_',
                     jid = jid:to_lower(UserJID),
                     node = node()}).

-spec watchers(class(), ejabberd:jid()) -> [ejabberd:jid()].
watchers(Class, Object) ->
    BareWatchers = mnesia:dirty_match_object(
                     table(Class),
                     #watcher{object = jid:to_lower(Object), _ = '_'}),
    lists:map(fun(#watcher{jid = SJID}) -> jid:make(SJID) end, BareWatchers).


%%%===================================================================
%%% MIM hook handlers
%%%===================================================================

-spec node_cleanup_hook(class(), ejabberd:lresource()) -> ok.
node_cleanup_hook(Class, Node) ->
    cleanup(Class, #watcher{node = Node, _ = '_'}).

-spec unset_presence_hook(class(), ejabberd:luser(),
                          ejabberd:lserver(), ejabberd:lresource(),
                          binary()) -> ok.
unset_presence_hook(Class, User, Server, Resource, _Status) ->
    unwatch_all(Class, jid:make(User, Server, Resource)).

%%%===================================================================
%%% Helpers
%%%===================================================================

cleanup(Class, Pattern) ->
    Table = table(Class),
    ToClean = mnesia:dirty_match_object(Table, Pattern),
    lists:foreach(mnesia:dirty_delete_object(Table, _), ToClean).

table(Class) ->
    list_to_atom(atom_to_list(Class) ++ "_watcher_table").

make_record(User, Object) ->
    #watcher{jid = jid:to_lower(User),
             object = jid:to_lower(Object),
             node = node()}.
