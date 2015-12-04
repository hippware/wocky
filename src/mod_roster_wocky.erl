%%% @copyright 2015+ Hippware, Inc.
%%% @doc Cassandra based implementation of mod_roster

-module(mod_roster_wocky).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/mod_roster.hrl").

%% API
-behaviour(mod_roster).
-export([init/2,
         transaction/2,
         read_roster_version/2,
         write_roster_version/4,
         get_roster/2,
         get_roster_by_jid_t/3,
         get_subscription_lists/3,
         roster_subscribe_t/4,
         get_roster_by_jid_with_groups_t/3,
         remove_user/2,
         update_roster_t/4,
         del_roster_t/3,
         read_subscription_and_groups/3,
         raw_to_record/2]).


%%%===================================================================
%%% API
%%%===================================================================

-spec init(ejabberd:server(), list()) -> ok.
init(_Host, _Opts) ->
    ok.

-spec transaction(ejabberd:lserver(), fun())
                 -> {aborted, any()} | {atomic, any()}.
transaction(_LServer, _F) ->
    {aborted, []}.

-spec read_roster_version(ejabberd:luser(), ejabberd:lserver())
                         -> binary() | error.
read_roster_version(_LUser, _LServer) ->
    error.

-spec write_roster_version(ejabberd:luser(), ejabberd:lserver(),
                           boolean(), binary()) -> any().
write_roster_version(_LUser, _LServer, _InTransaction, _Ver) ->
    ok.

-spec get_roster(ejabberd:luser(), ejabberd:lserver()) -> [mod_roster:roster()].
get_roster(_LUser, _LServer) ->
    [].

-spec get_roster_by_jid_t(ejabberd:luser(), ejabberd:lserver(),
                          ejabberd:simple_jid()) -> term().
get_roster_by_jid_t(_LUser, _LServer, _LJid) ->
    ok.

-spec get_subscription_lists(term(), ejabberd:luser(), ejabberd:lserver())
                            -> term().
get_subscription_lists(_Acc, _LUser, _LServer) ->
    ok.

-spec roster_subscribe_t(ejabberd:luser(), ejabberd:lserver(),
                         ejabberd:simple_jid(), mod_roster:roster()) -> term().
roster_subscribe_t(_LUser, _LServer, _LJid, _SJid) ->
    ok.

-spec get_roster_by_jid_with_groups_t(ejabberd:luser(), ejabberd:lserver(),
                                      ejabberd:simple_jid()) -> term().
get_roster_by_jid_with_groups_t(_LUser, _LServer, _LJid) ->
    ok.

-spec remove_user(ejabberd:luser(), ejabberd:lserver()) -> term().
remove_user(_LUser, _LServer) ->
    ok.

-spec update_roster_t(ejabberd:luser(), ejabberd:lserver(),
                      ejabberd:simple_jid(), mod_roster:roster()) -> term().
update_roster_t(_LUser, _LServer, _LJid, _Item) ->
    ok.

-spec del_roster_t(ejabberd:luser(), ejabberd:lserver(), ejabberd:simple_jid())
                  -> term().
del_roster_t(_LUser, _LServer, _LJid) ->
    ok.

-spec read_subscription_and_groups(ejabberd:luser(), ejabberd:lserver(),
                                   ejabberd:simple_jid()) -> term().
read_subscription_and_groups(_LUser, _LServer, _LJid) ->
    ok.

-spec raw_to_record(ejabberd:lserver(), term()) -> error | mod_roster:roster().
raw_to_record(_LServer, _Item) ->
    ok.
