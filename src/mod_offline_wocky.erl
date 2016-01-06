%%% @copyright 2015+ Hippware, Inc.
%%% @doc Cassandra based implementation of mod_offline

-module(mod_offline_wocky).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/mod_offline.hrl").

%% API
-behaviour(mod_offline).
-export([init/2,
         pop_messages/2,
         write_messages/4,
         remove_expired_messages/1,
         remove_old_messages/2,
         remove_user/2]).


%%%===================================================================
%%% API
%%%===================================================================

-spec init(ejabberd:server(), list()) -> ok.
init(_Host, _Opts) ->
    ok.

-spec pop_messages(ejabberd:luser(), ejabberd:lserver()) ->
    {ok, list(#offline_msg{})} | {error, term()}.
pop_messages(_LUser, _LServer) ->
    {ok, []}.

-spec write_messages(ejabberd:luser(), ejabberd:lserver(), list(), integer()) ->
    ok | {dicarded, list(#offline_msg{})} | {error, term()}.
write_messages(_LUser, _LServer, _Msgs, _MaxOfflineMsgs) ->
    ok.

-spec remove_expired_messages(ejabberd:lserver()) -> {error, term()} | {ok, integer()}.
remove_expired_messages(_Host) ->
    {ok, 0}.

-spec remove_old_messages(ejabberd:lserver(), integer()) -> {error, term()} | {ok, integer()}.
remove_old_messages(_Host, _Days) ->
    {ok, 0}.

-spec remove_user(binary(), binary()) -> any().
remove_user(_User, _Server) ->
    ok.
