%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_cassandra.erl
%%% Author  : Beng Tan
%%% Purpose : Authentication via cassandra
%%% Copyright (c) 2015 Hippware
%%%
%%%
%%% For schema, see priv/schema.cql
%%%
%%% Enable with the following in ejabberd.cfg
%%% {auth_method, cassandra}. 
%%%----------------------------------------------------------------------

-module(ejabberd_auth_cassandra).

% -behaviour(ejabberd_gen_auth).
-export([start/1,
         stop/1,
         try_register/3,
         get_vh_registered_users_number/1,
         get_vh_registered_users_number/2,
         is_user_exists/2
         ]).

-include("ejabberd.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec start(Host :: ejabberd:server()) -> ok.
start(_Host) ->
    ok.

-spec stop(Host :: ejabberd:server()) -> ok.
stop(_Host) ->
    ok.

% This is invoked from ejabberd_auth:try_register().
% Not really suitable for use since it does not pass in extra profile information.
% Exists for completeness more than anything else (at the moment)
-spec try_register(User :: ejabberd:user(),
                   Server :: ejabberd:server(),
                   Password :: binary()
                   ) -> {atomic, ok | exists}
                      | {error, invalid_jid | not_allowed}
                      | {aborted, _}.
try_register(User, Server, Password) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    if
        (LUser == error) or (LServer == error) ->
            {error, invalid_jid};
        true ->
            Id = cassandra:timeuuid(Server),
            {ok, Return} = cassandra:pquery(shared, <<"INSERT INTO username_to_user (username, id, domain) VALUES (?, ?, ?) IF NOT EXISTS">>, [LUser, Id, LServer], quorum),
            Result = cassandra:single_result(Return),
            % Note: Result is <<1>> for success, <<0>> if error. 
            % There is no documentation on the return type so it's possible, 
            %   in the future, this may not be a binary. 
            Success = Result /= <<0>>,

            case Success of
                true -> 
                    Password2 = case scram:enabled(Server) and is_binary(Password) of
						true -> 
                            scram:password_to_scram(Password, scram:iterations(Server));
						false ->
                            % Store plaintext passwords inside scram fields with iteration = 0 (which is an invalid value)
                            #scram{storedkey = Password,
                                serverkey = <<"">>,
                                salt = <<"">>,
                                iterationcount = 0}
                    end,
                    
                    {ok, _} = cassandra:pquery(LServer, <<"INSERT INTO user (id, domain, username, stored_key, server_key, salt, iteration_count) VALUES (?, ?, ?, ?, ?, ?, ?)">>, 
                        [Id, LServer, LUser, Password2#scram.storedkey, Password2#scram.serverkey, Password2#scram.salt, Password2#scram.iterationcount], quorum),
                    {atomic, ok};
                false ->
                    {atomic, exists}
            end
    end.

-spec get_vh_registered_users_number(Server :: ejabberd:server()
                                    ) -> non_neg_integer().
get_vh_registered_users_number(Server) ->
    % Let's not do a SELECT COUNT(*)
    -1.

get_vh_registered_users_number(Server, _) ->
    get_vh_registered_users_number(Server).

-spec is_user_exists(User :: ejabberd:user(),
                     Server :: ejabberd:server()
                     ) -> boolean() | {error, atom()}.
is_user_exists(User, _Server) ->
    LUser = jlib:nodeprep(User),
    {ok, Return} = cassandra:pquery(shared, <<"SELECT username, id, domain FROM username_to_user WHERE username = ?">>, [LUser], quorum),
    Result = cassandra:rows(Return),
    length(Result) > 0.


%% @doc gen_auth unimplemented callbacks
login(_User, _Server) -> erlang:error(not_implemented).
get_password(_User, _Server, _DefaultValue) -> erlang:error(not_implemented).
