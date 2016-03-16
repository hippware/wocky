%%% @copyright 2016+ Hippware, Inc.
%%% @doc Wocky user registration server
%%%
%%% This module implements the Wocky registration protocol described at
%%% https://github.com/hippware/tr-wiki/wiki/Registration-Protocol
-module(wocky_rest_reg).

%% Webmachine callbacks
-export([init/1,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         malformed_request/2,
         forbidden/2,
         post_is_create/2,
         create_path/2,
         from_json/2
        ]).

-ifdef(TEST).
-export([verify_session/2]).
-endif.

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-compile({parse_transform, do}).

-record(state, {
          server                   :: binary(),
          auth_providers           :: [string()],
          auth_bypass_prefixes     :: [binary()],
          is_new           = false :: boolean(),
          handle_set       = false :: boolean(),
          phone_number_set = false :: boolean(),
          fields                   :: map(), % Parsed request fields
          create_allowed   = false :: boolean()
         }).

%%%===================================================================
%%% Webmachine callbacks
%%%===================================================================

init(Opts) ->
    AuthProviders = proplists:get_value(auth_providers, Opts),
    AuthBypassPrefixes = proplists:get_value(auth_bypass_prefixes, Opts, []),
    Server = proplists:get_value(server, Opts),
    {ok, #state{
            auth_providers = AuthProviders,
            auth_bypass_prefixes = AuthBypassPrefixes,
            server = Server
           }}.

allowed_methods(RD, Ctx) ->
    {['POST'], RD, Ctx}.

content_types_accepted(RD, Ctx) ->
    {[{"application/json", from_json}], RD, Ctx}.

content_types_provided(RD, Ctx) ->
    {[{"application/json", undefined}], RD, Ctx}.

malformed_request(RD, Ctx = #state{server = Server}) ->
    ok = lager:info("Received REST request: ~p", [wrq:req_body(RD)]),
    case parse_request(wrq:req_body(RD), Server) of
        {ok, Fields} ->
            {false, RD, Ctx#state{fields = Fields}};
        {error, {Code, Error}} ->
            ok = lager:info("Error in parsing phase: ~p ~p", [Code, Error]),
            RD2 = set_resp_body(Code, Error, RD),
            {true, RD2, Ctx}
    end.

forbidden(RD, Ctx = #state{fields = Fields,
                           auth_providers = AuthProviders,
                           auth_bypass_prefixes = AuthBypassPrefixes
                          }) ->
    case authenticate(Fields, AuthProviders, AuthBypassPrefixes) of
        {true, digits} ->
            {false, RD, Ctx#state{create_allowed = true}};
        {true, session} ->
            {false, RD, Ctx#state{fields = maps:remove(phoneNumber, Fields)}};
        {false, Code, Error} ->
            RD2 = set_resp_body(Code, Error, RD),
            {true, RD2, Ctx}
    end.

post_is_create(RD, Ctx) -> {true, RD, Ctx}.

create_path(RD, Ctx) -> {"", RD, Ctx}.

-spec from_json(#wm_reqdata{}, #state{}) ->
    {true, #wm_reqdata{}, #state{}}.
from_json(RD, Ctx = #state{create_allowed = true}) ->
    create_or_update_user(RD, Ctx);
from_json(RD, Ctx) ->
    find_and_update_user(RD, Ctx).

%%%===================================================================
%%% Request processing helper functions
%%%===================================================================

parse_request(Body, Server) ->
    do([error_m ||
        Elements <- decode_json(Body),
        Fields <- {ok, wocky_rest:map_keys_to_atoms(maps:from_list(Elements))},
        Fields2 <- {ok, maybe_add_default_server(Fields, Server)},
        verify_auth_fields(Fields2),
        verify_user_fields(Fields2),
        verify_avatar_field(Fields2),
        {ok, Fields2}
       ]).

decode_json(Body) ->
    try mochijson2:decode(Body) of
        {struct, Elements} -> {ok, Elements}
    catch
        error:_ -> {error, {400, "Could not parse JSON"}}
    end.

verify_auth_fields(#{sessionID := _}) -> ok;
verify_auth_fields(#{'X-Auth-Service-Provider'            := _,
                     'X-Verify-Credentials-Authorization' := _,
                     phoneNumber                          := _
                    }) -> ok;
verify_auth_fields(_) -> {error, {400, "Missing authentication data"}}.

verify_user_fields(#{uuid     := UUID,
                     resource := _
                    }) ->
    case wocky_db_user:is_valid_id(UUID) of
        true -> ok;
        false -> {error, {400, "Invalid UUID"}}
    end;
verify_user_fields(#{userID   := _,
                     resource := _
                    }) -> ok;
verify_user_fields(_) -> {error, {400, "Missing user identifier or resource"}}.

verify_avatar_field(#{avatar := Avatar,
                      server := Server}) ->
    case hxep:parse_url(Avatar) of
        {ok, {Server, FileID}} -> verify_file_id(FileID);
        _ -> {error, {400, "Invalid or non-local avatar URL"}}
    end;
verify_avatar_field(_) -> ok.

verify_file_id(FileID) ->
    case francus:is_valid_id(FileID) of
        true -> ok;
        false -> {error, {400, "Invalid file ID in URL"}}
    end.

maybe_add_default_server(Fields = #{server := _}, _) -> Fields;
maybe_add_default_server(Fields, Server) -> Fields#{server => Server}.

authenticate(
    #{
      'X-Auth-Service-Provider'            := AuthProvider,
      'X-Verify-Credentials-Authorization' := Auth,
      phoneNumber                          := PhoneNumber
     }, AuthProviders, AuthBypassPrefixes) ->
    case has_any_prefix(PhoneNumber, AuthBypassPrefixes) of
        true ->
            {true, digits};
        false ->
            verify_digits_auth(Auth, PhoneNumber, AuthProvider, AuthProviders)
    end;
authenticate(
    Fields = #{
      sessionID := SessionID
     }, _, _) ->
    case verify_session(Fields, SessionID) of
        true ->
            {true, session};
        false ->
            ok = lager:info("Invalid sessionID: ~p", [SessionID]),
            {false, 401, "Invalid sessionID"}
    end.

verify_digits_auth(Auth, PhoneNumber, AuthProvider, AuthProviders) ->
    case verify_auth(Auth, PhoneNumber, AuthProvider, AuthProviders) of
        true ->
            {true, digits};
        {false, Code, Error} ->
            ok = lager:info("Failed digits auth: ~p ~p", [Code, Error]),
            {false, Code, Error}
    end.

% Check that the auth server is one that we have configured as valid
verify_auth(Auth, PhoneNumber, AuthProvider, ValidProviders) ->
    case lists:member(binary_to_list(AuthProvider), ValidProviders) of
        true ->
            wocky_digits_auth:verify(Auth, PhoneNumber, AuthProvider);
        false ->
            ok = lager:info("Invalid authentication provider: ~p",
                            [AuthProvider]),
            {false, 401, "Invalid authentication provider"}
    end.

verify_session(#{uuid := UUID, server := Server, resource := Resource},
               SessionID) ->
    wocky_db_user:check_token(UUID, Server, Resource, SessionID);
verify_session(Fields = #{userID := UserID, server := Server}, SessionID) ->
    case wocky_db_user:get_user_by_auth_name(Server, UserID) of
        not_found ->
            false;
        User ->
            verify_session(Fields#{uuid => User}, SessionID)
    end.

create_or_update_user(RD, Ctx = #state{fields = Fields
                                              = #{userID := AuthUser,
                                                  server := Server}}) ->
    case wocky_db_user:get_user_by_auth_name(Server, AuthUser) of
        not_found ->
            create_user(RD, Ctx);
        ExistingUser ->
            find_and_update_user(RD,
                                 Ctx#state{fields =
                                           Fields#{uuid => ExistingUser}})
    end.

create_user(RD, Ctx = #state{fields = Fields}) ->
    case wocky_db_user:create_user(json_to_row(Fields)) of
        {error, E} -> create_update_error(E, RD, Ctx);
        UUID ->
            finalize_changes(RD, Ctx#state{is_new = true,
                                           fields = Fields#{uuid => UUID}})
    end.

find_and_update_user(RD, Ctx = #state{fields = #{uuid := _}}) ->
    update_user(RD, Ctx);
find_and_update_user(RD, Ctx = #state{fields = Fields
                                             = #{userID := AuthUser,
                                                 server := Server}}) ->
    UUID = wocky_db_user:get_user_by_auth_name(Server, AuthUser),
    update_user(RD, Ctx#state{fields = Fields#{uuid => UUID}}).

update_user(RD, Ctx = #state{fields = Fields}) ->
    case wocky_db_user:update_user(json_to_row(Fields)) of
        ok -> finalize_changes(RD, Ctx);
        {error, E} -> create_update_error(E, RD, Ctx)
    end.

finalize_changes(RD, Ctx) ->
    Ctx2 = maybe_update_handle(Ctx),
    Ctx3 = maybe_update_phone_number(Ctx2),
    set_result(RD, Ctx3).

maybe_update_handle(Ctx = #state{server = Server,
                                 fields = #{uuid := User,
                                            handle := Handle}}) ->
    Set = wocky_db_user:maybe_set_handle(User, Server, Handle),
    Ctx#state{handle_set = Set};
maybe_update_handle(Ctx) -> Ctx.

maybe_update_phone_number(Ctx = #state{server = Server,
                                       fields = #{uuid := User,
                                                  phoneNumber
                                                  := PhoneNumber}}) ->
    wocky_db_user:set_phone_number(User, Server, PhoneNumber),
    Ctx#state{phone_number_set = true};
maybe_update_phone_number(Ctx) -> Ctx.

set_result(RD, Ctx = #state{server = Server,
                            is_new = IsNew,
                            phone_number_set = PhoneNumberSet,
                            handle_set = HandleSet,
                            fields = #{uuid := UUID,
                                       resource := Resource}}) ->
    Fields = row_to_json(
               wocky_db:drop_nulls(
                 wocky_db_user:get_user_data(UUID, Server))),
    JSONFields = prepare_for_encoding(Fields),
    {ok, Token} = wocky_db_user:assign_token(UUID, Server, Resource),
    Ret = [{sessionID, Token}, {isNew, IsNew},
           {phoneNumberSet, PhoneNumberSet}, {handleSet, HandleSet},
           {resource, Resource} | maps:to_list(JSONFields)],
    Body = mochijson2:encode({struct, Ret}),
    RD2 = wrq:set_resp_header("content-type", "application/json", RD),
    RD3 = wrq:set_resp_body(Body, RD2),

    ok = lager:info("Operation complete. Replying: ~p", [Body]),
    {true, RD3, Ctx}.

create_update_error(E, RD, Ctx) ->
    RD2 = wrq:set_resp_header("content-type", "application/json", RD),
    RD3 = set_resp_body(409, atom_to_list(E), RD2),

    ok = lager:info("Error in operation: ~p", [E]),
    {{halt, 409}, RD3, Ctx}.


%%%===================================================================
%%% Helper funcitons
%%%===================================================================

field_mappings() ->
      %JSON Tag     %DB field name
    [{userID,       auth_user},
     {uuid,         user},
     {server,       server},
     {handle,       handle},
     {firstName,    first_name},
     {lastName,     last_name},
     {phoneNumber,  phone_number},
     {email,        email},
     {avatar,       avatar}
     % Strip all other fields
    ].

json_to_row(JSONFields) ->
    lists:foldl(fun({J, R}, Map) -> map_transform(J, R, JSONFields, Map) end,
                #{}, field_mappings()).

row_to_json(DBFields) ->
    lists:foldl(fun({J, R}, Map) -> map_transform(R, J, DBFields, Map) end,
                #{}, field_mappings()).

map_transform(A, B, SourceMap, Map) ->
    case maps:find(A, SourceMap) of
        {ok, Val} -> Map#{B => Val};
        error -> Map
    end.

prepare_for_encoding(Fields = #{uuid := UUID}) ->
    Fields#{uuid => UUID}.

set_resp_body(Code, Error, RD) ->
    JSON = mochijson2:encode({struct, [{code, Code},
                                       {error, list_to_binary(Error)}]}),
    wrq:set_resp_body(JSON, RD).

has_any_prefix(PhoneNumber, Prefixes) ->
    lists:any(fun(Prefix) -> has_prefix(PhoneNumber, Prefix) end,
              Prefixes).

has_prefix(Subject, Prefix) ->
    binary:longest_common_prefix([Subject, Prefix]) =:= byte_size(Prefix).
