%%% @copyright 2015+ Hippware, Inc.
%%% @doc Cassandra-backed authentication backend
%%%
%%% This module serves a number of purposes: 
%%%
%%% 1) Pluggable ejabberd authentication backend ({@link ejabberd_gen_auth})
%%% 2) Application specific user functionality
%%%
%%% Complications arise because they are not identical.
%%%
%%% Ejabberd users are a (localpart, domainpart) pair with localpart being the 'username'.
%%% Wocky users are a (localpart, domainpart, username) tuple with username being a separate quantity (which is also globally unique across all domains).
%%%
%%% In order to utilise existing code, this module needs to conform to {@link ejabberd_gen_auth} but not all of the functions required of (1) make sense for (2). Hence, for those functions which don't make sense, a "best effort" implementation which is "least surprising" will have to suffice. In other words, all the functions of (1) need to be implemented, but not all of them will be useful or are expected to be used in normal operations. 
%%%
%%%
%%% For schema, see priv/schema*.cql
%%%
%%% Enable with the following in ejabberd.cfg
%%%
%%% ```
%%% {auth_method, cassandra}. 
%%% '''

-module(ejabberd_auth_cassandra).

% -behaviour(ejabberd_gen_auth).
-export([start/1,
         stop/1,
         try_register/3,
         get_vh_registered_users_number/1,
         get_vh_registered_users_number/2,
         is_user_exists/2,
         remove_user/3
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
get_vh_registered_users_number(_Server) ->
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

%% @doc Remove user if the provided password is correct.
%% This functionality is unused (only called from mod_register) so just stub it out. 
-spec remove_user(User :: ejabberd:user(),
                  Server :: ejabberd:server(),
                  Password :: binary()
                  ) -> ok | not_exists | not_allowed | bad_request | error.
remove_user(User, Server, _Password) ->
    case is_user_exists(User, Server) of
        true -> not_allowed;
        false -> not_exists
    end.

%% @doc gen_auth unimplemented callbacks
login(_User, _Server) -> erlang:error(not_implemented).
get_password(_User, _Server, _DefaultValue) -> erlang:error(not_implemented).
