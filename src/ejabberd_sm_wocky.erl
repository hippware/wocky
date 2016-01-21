%%% @copyright 2015+ Hippware, Inc.
%%% @doc Cassandra based implementation of mod_offline

%% Structure of a session record:
%%
%%          {session,
%% sid:       {{1453,332132,605544},<0.679.0>},
%% usr:       {<<"699bf62c-b897-11e5-9912-ba0be0483c18">>,
%%             <<"localhost">>,
%%             <<"15938181821453332132592057">>},
%% us:        {<<"699bf62c-b897-11e5-9912-ba0be0483c18">>,
%%             <<"localhost">>},
%% priority:  1,
%% info:      [{ip,{{127,0,0,1},41127}},
%%             {conn,c2s_tls},
%%             {auth_module,ejabberd_auth_wocky}]},

-module(ejabberd_sm_wocky).

-behavior(ejabberd_gen_sm).

-include_lib("ejabberd/include/ejabberd.hrl").

-export([start/1,
         get_sessions/0,
         get_sessions/1,
         get_sessions/2,
         get_sessions/3,
         create_session/4,
         delete_session/4,
         cleanup/1,
         total_count/0,
         unique_count/0]).

-spec start(list()) -> any().
start(_Opts) ->
    ok.

% `get_sessions/0' is only used by testing and extra admin tools. It doesn't
% make sense in our setup because it gives us no keyspace to work with, so it's
% not implmented.
-spec get_sessions() -> [].
get_sessions() ->
    [].

-spec get_sessions(ejabberd:server()) -> [ejabberd_sm:ses_tuple()].
get_sessions(Server) ->
    Q = "SELECT * FROM session",
    {ok, Result} = wocky_db:query(Server, Q, [], quorum),
    [row_to_ses_tuple(R) || R <- wocky_db:rows(Result)].

-spec get_sessions(ejabberd:user(), ejabberd:server()) ->
    [ejabberd_sm:session()].
get_sessions(User, Server) ->
    SIDBins = user_sids(Server, User),
    Queries = [session_query(SB) || SB <- SIDBins],
    sessions_from_queries(Server, Queries).

-spec get_sessions(ejabberd:user(), ejabberd:server(), ejabberd:resource()
                  ) -> [ejabberd_sm:session()].
get_sessions(User, Server, Resource) ->
    SIDBins = user_sids(Server, User),
    Queries = [session_query(SB, Resource) || SB <- SIDBins],
    sessions_from_queries(Server, Queries).

-spec create_session(ejabberd:user(),
                     ejabberd:server(),
                     ejabberd:resource(),
                     ejabberd_sm:session()) -> ok.
create_session(_User, Server, _Resource, Session) ->
    Queries = [add_session_data_query(Session), add_session_map_query(Session)],
    {ok, void} = wocky_db:batch_query(Server, Queries, logged, quorum),
    ok.

-spec delete_session(ejabberd_sm:sid(),
                     ejabberd:user(),
                     ejabberd:server(),
                     ejabberd:resource()) -> ok.
delete_session(SID, User, Server, _Resource) ->
    SIDBin = term_to_binary(SID),
    Queries = delete_session_queries(SIDBin, User),
    {ok, void} = wocky_db:batch_query(Server, Queries, logged, quorum),
    ok.

-spec cleanup(atom()) -> ok.
cleanup(Node) ->
    lists:foreach(fun(D) -> cleanup_server(D, Node) end, servers()).

cleanup_server(Server, Node) ->
    SidsUsers = sids_users_on_node(Server, Node),
    QuerySets = [delete_session_queries(S, U)
                 || #{sid := S, jid_user := U} <- SidsUsers],
    lists:foreach(fun(Q) ->
                          {ok, void} = wocky_db:batch_query(Server, Q,
                                                            logged, quorum)
                  end,
                  QuerySets).

-spec total_count() -> 0.
total_count() ->
    lists:sum(
      lists:map(fun count_on_server/1, servers())).

-spec unique_count() -> 0.
unique_count() ->
    length(
      lists:usort(
        lists:flatten(
          lists:map(fun uss_on_server/1, servers())))).

sids_users_on_node(Server, Node) ->
    Q = "SELECT sid, jid_user FROM session WHERE node = ?",
    V = #{node => atom_to_list(Node)},
    {ok, Result} = wocky_db:query(Server, Q, V, quorum),
    wocky_db:rows(Result).

delete_session_queries(SIDBin, User) ->
    [delete_session_data_query(SIDBin), delete_session_map_query(SIDBin, User)].

sessions_from_queries(Server, Queries) ->
    Rows = lists:foldl(
      fun({Q, V}, Acc) ->
              {ok, Results} = wocky_db:query(Server, Q, V, quorum),
              wocky_db:rows(Results) ++ Acc
      end,
      [],
      Queries),

    [row_to_rec(R) || R <- Rows].

user_sids(Server, User) ->
    {Q, V} = sessions_query(User),
    {ok, Result} = wocky_db:query(Server, Q, V, quorum),
    case wocky_db:single_row(Result) of
        #{sids := SIDBins} -> SIDBins;
        #{} -> []
    end.

session_query(SIDBin) ->
    {"SELECT * FROM session WHERE sid = ?",
     #{sid => SIDBin}}.

session_query(SIDBin, Resource) ->
    {"SELECT * FROM session WHERE sid = ? AND jid_resource = ? ALLOW FILTERING",
     #{sid => SIDBin, jid_resource => Resource}}.

sessions_query(User) ->
    {"SELECT sids FROM user_to_sids WHERE jid_user = ?",
     #{jid_user => User}}.

add_session_data_query(Session) ->
    {"INSERT INTO session (sid, node, user, server, jid_user, jid_server,
        jid_resource, priority, info) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
     rec_to_row(Session)}.

add_session_map_query(Session) ->
    {"UPDATE user_to_sids SET sids = sids + ? WHERE jid_user = ?",
     #{jid_user => jid_user(Session),
       sids => [term_to_binary(Session#session.sid)]}}.

delete_session_data_query(SIDBin) ->
    {"DELETE FROM session WHERE sid = ?",
     #{sid => SIDBin}}.

delete_session_map_query(SIDBin, User) ->
    {"UPDATE user_to_sids SET sids = sids - ? where jid_user = ?",
     #{jid_user => User, sids => [SIDBin]}}.

rec_to_row(#session{
              sid = SID,
              usr = {User, Server, Resource},
              us = {LUser, LServer},
              priority = Priority,
              info = Info}) ->
    #{sid => term_to_binary(SID),
      node => atom_to_list(sid_node(SID)),
      user => LUser,
      server => LServer,
      jid_user => User,
      jid_server => Server,
      jid_resource => Resource,
      priority => pri_to_int(Priority),
      info => term_to_binary(Info)}.

row_to_rec(#{sid := SID,
             user := LUser,
             server := LServer,
             jid_user := User,
             jid_server := Server,
             jid_resource := Resource,
             priority := Priority,
             info := Info}) ->
    #session{
       sid = binary_to_term(SID),
       usr = {wocky_db_user:import_id(User), Server, Resource},
       us = {wocky_db_user:import_id(LUser), LServer},
       priority = int_to_pri(Priority),
       info = binary_to_term(Info)}.

% Create an ejabberd_sm:ses_tuple() from a row
row_to_ses_tuple(#{sid := SID,
                   jid_user := User,
                   jid_server := Server,
                   jid_resource := Resource,
                   priority := Priority,
                   info := Info}) ->
    {{wocky_db_user:import_id(User), Server, Resource},
     binary_to_term(SID),
     int_to_pri(Priority),
     binary_to_term(Info)}.

jid_user(#session{usr = {User, _Server, _Resource}}) -> User.

count_on_server(Server) ->
    Q = "SELECT sid FROM session",
    {ok, Results} = wocky_db:query(Server, Q, [], quorum),
    length(wocky_db:rows(Results)).

uss_on_server(Server) ->
    Q = "SELECT user, server FROM session",
    {ok, Results} = wocky_db:query(Server, Q, [], quorum),
    [{wocky_db_user:import_id(U), S} || #{user := U, server := S}
                                        <- wocky_db:rows(Results)].

% Priority can be undefined. Store this as -1 (an otherwise invalid priority)
pri_to_int(undefined) -> -1;
pri_to_int(N) -> N.

int_to_pri(-1) -> undefined;
int_to_pri(N) -> N.

sid_node({_Now, Pid}) -> node(Pid).

servers() ->
    ejabberd_config:get_global_option(hosts).
