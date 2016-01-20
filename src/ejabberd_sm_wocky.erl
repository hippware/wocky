%%% @copyright 2015+ Hippware, Inc.
%%% @doc Cassandra based implementation of mod_offline

-module(ejabberd_sm_wocky).

-behavior(ejabberd_gen_sm).

-include_lib("ejabberd/include/ejabberd.hrl").

%{session,{{1453,178354,877349},<0.693.0>},
%           {<<"c3de828a-bda6-11e5-b2fe-080027f70e96">>,<<"localhost">>,
%            <<"10900204961453178354876621">>},
%           {<<"c3de828a-bda6-11e5-b2fe-080027f70e96">>,<<"localhost">>},
%           1,
%           [{ip,{{127,0,0,1},40326}},
%            {conn,c2s_tls},
%            {auth_module,ejabberd_auth_wocky}]}],

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
    Q = "SELECT * FROM session where jid_user = ?",
    Values = #{jid_user => User},
    query_recs(Server, Q, Values).

-spec get_sessions(ejabberd:user(), ejabberd:server(), ejabberd:resource()
                  ) -> [ejabberd_sm:session()].
get_sessions(User, Server, Resource) ->
    Q = "SELECT * FROM session where jid_user = ?, jid_resource = ?",
    Values = #{jid_user => User, jid_resource => Resource},
    query_recs(Server, Q, Values).

-spec create_session(ejabberd:user(),
                     ejabberd:server(),
                     ejabberd:resource(),
                     ejabberd_sm:session()) -> ok.
create_session(_User, Server, _Resource, Session) ->
    Q = "INSERT INTO session (sid, user, server, jid_user, jid_server,
        jid_resource, priority, info) VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
    {ok, void} = wocky_db:query(Server, Q, rec_to_row(Session), quorum),
    ok.

-spec delete_session(ejabberd_sm:sid(),
                     ejabberd:user(),
                     ejabberd:server(),
                     ejabberd:resource()) -> ok.
delete_session(SID, _User, Server, _Resource) ->
    Q = "DELETE FROM session WHERE sid = ?",
    Values = #{sid => term_to_binary(SID)},
    {ok, void} = wocky_db:query(Server, Q, Values, quorum),
    ok.

-spec cleanup(atom()) -> ok.
cleanup(Node) ->
    Q = "DELETE FROM session WHERE node = ?",
    Values = #{node => atom_to_list(Node)},
    lists:foreach(fun(Domain) ->
                          {ok, void} = wocky_db:query(Domain, Q, Values, quorum)
                  end,
                  domains()).

-spec total_count() -> 0.
total_count() ->
    0.

-spec unique_count() -> 0.
unique_count() ->
    0.

query_recs(Server, Q, Values) ->
    {ok, Result} = wocky_db:query(Server, Q, Values, quorum),
    [row_to_rec(R) || R <- wocky_db:rows(Result)].


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

sid_node({_Now, Pid}) -> node(Pid).

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
       usr = {User, Server, Resource},
       us = {LUser, LServer},
       priority = int_to_pri(Priority),
       info = binary_to_term(Info)}.

% Create an ejabberd_sm:ses_tuple() from a row
row_to_ses_tuple(#{sid := SID,
                   jid_user := User,
                   jid_server := Server,
                   jid_resource := Resource,
                   priority := Priority,
                   info := Info}) ->
    {{User, Server, Resource},
     binary_to_term(SID),
     int_to_pri(Priority),
     binary_to_term(Info)}.

% Priority can be undefined. Store this as -1 (an otherwise invalid priority)
pri_to_int(undefined) -> -1;
pri_to_int(N) -> N.

int_to_pri(-1) -> undefined;
int_to_pri(N) -> N.

domains() ->
    ejabberd_config:get_global_option(hosts).
