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
    {ok, [#offline_msg{}]}.
pop_messages(LUser, LServer) ->
    Rows = get_user_messages(LUser, LServer),
    purge_messages(LServer, Rows),
    Recs = [ row_to_rec(LUser, LServer, Row) || Row <- Rows ],
    {ok, Recs}.

-spec write_messages(ejabberd:luser(), ejabberd:lserver(),
                     [#offline_msg{}], integer()) -> ok.
write_messages(LUser, LServer, Msgs, _MaxOfflineMsgs) ->
    % NOTE: The MaxOfflineMsgs value is not supported and will be ignored
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

%%%===================================================================
%%% Helper functions
%%%===================================================================

binary_to_xml(XML) ->
    {ok, Binary} = exml:parse(XML),
    Binary.

get_user_messages(LUser, LServer) ->
    Q = "SELECT * FROM offline_msg WHERE user = ?",
    Value = #{user => LUser},
    {ok, Results} = wocky_db:query(LServer, Q, Value, quorum),
    wocky_db:rows(Results).

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
