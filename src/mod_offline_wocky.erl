%%% @copyright 2015+ Hippware, Inc.
%%% @doc Cassandra based implementation of mod_offline

-module(mod_offline_wocky).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/mod_offline.hrl").

-type offline_msg() :: #offline_msg{}.
-export_type([offline_msg/0]).

%% API
-behaviour(mod_offline).
-export([init/2,
         pop_messages/2,
         write_messages/3,
         count_offline_messages/3,
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
    {ok, [offline_msg()]}.
pop_messages(LUser, LServer) ->
    Rows = get_user_messages(LUser, LServer),
    purge_messages(LServer, Rows),
    Recs = [ row_to_rec(LUser, LServer, Row) || Row <- Rows ],
    {ok, Recs}.

-spec write_messages(ejabberd:luser(), ejabberd:lserver(), [offline_msg()])
                    -> ok.
write_messages(LUser, LServer, Msgs) ->
    Q = "INSERT INTO offline_msg
            (user, server, msg_id, expire, timestamp, from_id, to_id, packet)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
    QTTL = Q ++ " USING TTL ?",
    QueryValues = [{
                case M#offline_msg.expire of
                    never -> Q;
                    _ -> QTTL
                end,
                rec_to_row(LUser, M)
              }
              || M <- Msgs
             ],
    wocky_db:multi_query(LServer, QueryValues, quorum).

-spec count_offline_messages(ejabberd:luser(), ejabberd:server(), integer())
                            -> integer().
count_offline_messages(_LUser, _LServer, _MaxToArchive) ->
    %% This function is used to implement offline messages thresholds/limits.
    %% We aren't supporting that feature at this time, and it isn't clear that
    %% we will need it. Returning 0 here satisfies the code in `mod_offline`
    %% and doesn't cause us obvious problems.
    0.

-spec remove_expired_messages(ejabberd:lserver()) ->
    {ok, integer()}.
remove_expired_messages(_Host) ->
    %% Expiry of messages is automatically handled by Cassandra's TTL system
    {ok, 0}.

-spec remove_old_messages(ejabberd:lserver(), erlang:timestamp()) ->
    {error, not_implemented}.
remove_old_messages(_Host, _Timestamp) ->
    {error, not_implemented}.

-spec remove_user(binary(), binary()) -> ok.
remove_user(User, Server) ->
    wocky_db:delete(Server, offline_msg, all, #{user => User}).


%%%===================================================================
%%% Helper functions
%%%===================================================================

binary_to_xml(XML) ->
    {ok, Binary} = exml:parse(XML),
    Binary.

get_user_messages(LUser, LServer) ->
    wocky_db:select(LServer, offline_msg, all, #{user => LUser}).

purge_messages(LServer, Rows) ->
    Q = "DELETE FROM offline_msg WHERE user = ?
            AND timestamp = ? AND msg_id = ?",
    wocky_db:multi_query(LServer, Q, Rows, quorum).

row_to_rec(LUser, LServer, #{timestamp := Timestamp, expire := Expire,
                             from_id := From, to_id := To, packet := Packet}) ->
    #offline_msg{
       us =         {LUser, LServer},
       timestamp =  wocky_db:timestamp_to_now(Timestamp),
       expire =     wocky_db:timestamp_to_now(Expire),
       from =       jid:from_binary(From),
       to =         jid:from_binary(To),
       packet =     binary_to_xml(Packet)
      }.

rec_to_row(LUser, #offline_msg{us = {_, LServer}, timestamp = Timestamp,
                               expire = Expire, from = From, to = To,
                               packet = Packet}) ->
    #{user =>       LUser,
      server =>     LServer,
      msg_id =>     ossp_uuid:make(v1, binary),
      timestamp =>  wocky_db:now_to_timestamp(Timestamp),
      expire =>     wocky_db:now_to_timestamp(Expire),
      from_id =>    jid:to_binary(From),
      to_id =>      jid:to_binary(To),
      packet =>     exml:to_binary(Packet),
      '[ttl]' =>    wocky_db:expire_to_ttl(Expire)
     }.
