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
    {ok, list(#offline_msg{})}.
pop_messages(LUser, LServer) ->
    Q1 = "SELECT * FROM offline_msg WHERE user = ?",
    Value = #{user => LUser},
    {ok, Results} = wocky_db:query(LServer, Q1, Value, quorum),
    Rows = wocky_db:rows(Results),
    Msgs = [
            #offline_msg{
               us = {LUser,LServer},
               timestamp = wocky_db:timestamp_to_seconds(
                             Timestamp),
               expire = wocky_db:timestamp_to_seconds(
                          Expire),
               from = From,
               to = To,
               packet = Packet
              }
            || #{timestamp := Timestamp, expire := Expire,
                 from_id := From, to_id := To, packet := Packet}
               <- Rows
           ],
    Q2 = "DELETE FROM offline_msg WHERE user = ?
            AND timestamp = ? AND msg_id = ?",
    wocky_db:multi_query(LServer, Q2, Rows, quorum),
    {ok, Msgs}.

-spec write_messages(ejabberd:luser(), ejabberd:lserver(),
                     [#offline_msg{}], integer()) -> ok.
write_messages(LUser, LServer, Msgs, _MaxOfflineMsgs) ->
    % NOTE: The MaxOfflineMsgs value is not supported and will be ignored
    Q = "INSERT INTO offline_msg
            (user, server, msg_id, expire, timestamp, from_id, to_id, packet)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?) USING TTL ?",
    Values = [#{user => LUser, server => LServer,
                 msg_id => ossp_uuid:make(v1, binary),
                 timestamp =>
                 wocky_db:seconds_to_timestamp(M#offline_msg.timestamp),
                 expire => wocky_db:seconds_to_timestamp(M#offline_msg.expire),
                 from_id => M#offline_msg.from,
                 to_id => M#offline_msg.to,
                 packet => M#offline_msg.packet,
                 '[ttl]' => wocky_db:expire_to_ttl(M#offline_msg.expire)}
              || M <- Msgs
             ],
    Expected = lists:duplicate(length(Msgs), {ok, void}),
    Expected = wocky_db:multi_query(LServer, Q, Values, quorum),
    ok.

-spec remove_expired_messages(ejabberd:lserver()) ->
    {ok, integer()}.
remove_expired_messages(_Host) ->
    % Expiry of messages is automatically handled by Cassandra's TTL system
    {ok, 0}.

-spec remove_old_messages(ejabberd:lserver(), integer()) ->
    {error, not_implemented}.
remove_old_messages(_Host, _Days) ->
    {error, not_implemented}.

-spec remove_user(binary(), binary()) -> ok.
remove_user(User, Server) ->
    Q = "DELETE FROM offline_msg WHERE user = ?",
    {ok, void} = wocky_db:query(Server, Q, #{user => User}, quorum),
    ok.
