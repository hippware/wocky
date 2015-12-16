%%% @copyright 2015+ Hippware, Inc.
%%% @doc A module to store XEP-0012 Last Activity data into Cassandra

-module(mod_last_wocky).

-behaviour(mod_last).

-export([init/2,
         get_last/2,
         count_active_users/2,
         set_last_info/4,
         remove_user/2]).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/mod_last.hrl").


%% @doc Initialise this module (from {@link mod_last})
-spec init(ejabberd:server(), list()) -> ok.
init(_Host, _Opts) ->
    ok.

-spec get_last(ejabberd:luser(), ejabberd:lserver())
              -> {ok, non_neg_integer(), binary()}
               | {error, term()}
               | not_found.
get_last(_LUser, _LServer) ->
    not_found.

-spec count_active_users(ejabberd:lserver(), non_neg_integer())
                        -> non_neg_integer().
count_active_users(_LServer, _TimeStamp) ->
    0.

-spec set_last_info(ejabberd:luser(), ejabberd:lserver(),
                    non_neg_integer(), binary())
                   -> ok | {error, term()}.
set_last_info(_LUser, _LServer, _TimeStamp, _Status) ->
    ok.

-spec remove_user(ejabberd:luser(), ejabberd:lserver()) -> ok.
remove_user(_LUser, _LServer) ->
    ok.
