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
-compile({parse_transform, fun_chain}).

-define(EMPTY,     <<>>).
-define(NOT_EMPTY, <<_:8, _/binary>>).

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
    ok = lager:info("Received registration request: ~s", [wrq:req_body(RD)]),
    case parse_request(wrq:req_body(RD), Server) of
        {ok, Fields} ->
            {false, RD, Ctx#state{fields = Fields}};
        {error, {Code, Error}} ->
            ok = lager:warning("Error parsing registration request: ~B ~s",
                               [Code, Error]),
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
from_json(RD, Ctx = #state{create_allowed = true, fields = Fields}) ->
    try
        create_or_update_user(RD, Ctx)
    catch
        Class:Reason ->
            create_internal_error(Class, Reason, Fields, RD, Ctx)
    end;
from_json(RD, Ctx = #state{fields = Fields}) ->
    try
        find_and_update_user(RD, Ctx)
    catch
        Class:Reason ->
            create_internal_error(Class, Reason, Fields, RD, Ctx)
    end.

%%%===================================================================
%%% Request processing helper functions
%%%===================================================================

parse_request(Body, Server) ->
    do([error_m ||
        Elements <- decode_json(Body),
        Fields <- elements_to_map(Elements, Server),
        verify_auth_fields(Fields),
        verify_user_fields(Fields),
        verify_avatar_field(Fields),
        strip_empty_fields(Fields)
       ]).

decode_json(Body) ->
    try mochijson2:decode(Body) of
        {struct, Elements} -> {ok, Elements}
    catch
        error:_ -> {error, {400, "Could not parse JSON"}}
    end.

elements_to_map(Elements, Server) ->
    Map = fun_chain:first(
            Elements,
            maps:from_list(),
            wocky_rest:map_keys_to_atoms(),
            maybe_add_default_server(Server),
            merge_defaults()
           ),
    {ok, Map}.

merge_defaults(Fields) ->
    Defaults = #{
      userID                               => ?EMPTY,
      uuid                                 => ?EMPTY,
      handle                               => ?EMPTY,
      firstName                            => ?EMPTY,
      lastName                             => ?EMPTY,
      resource                             => ?EMPTY,
      phoneNumber                          => ?EMPTY,
      email                                => ?EMPTY,
      sessionID                            => ?EMPTY,
      'X-Auth-Service-Provider'            => undefined,
      'X-Verify-Credentials-Authorization' => undefined
     },
    maps:merge(Defaults, Fields).

strip_empty_fields(Fields) ->
  {ok, maps:filter(fun (_, <<>>) -> false;
                       (_, undefined) -> false;
                       (_, _) -> true end,
                   Fields)}.

verify_auth_fields(#{phoneNumber := ?EMPTY}) ->
    {error, {400, "Missing or empty phoneNumber"}};
verify_auth_fields(#{'X-Auth-Service-Provider'            := X1,
                     'X-Verify-Credentials-Authorization' := X2,
                     sessionID                            := ?EMPTY})
  when X1 =:= undefined orelse X2 =:= undefined ->
    {error, {400, "The Digits fields or sessionID (or both) must be supplied"}};
verify_auth_fields(_) -> ok.

verify_user_fields(#{userID := ?EMPTY, uuid := ?EMPTY}) ->
    {error, {400, "userID or uuid (or both) must be supplied"}};
verify_user_fields(#{resource := ?EMPTY}) ->
    {error, {400, "resource must be supplied"}};
verify_user_fields(#{uuid := UUID}) when UUID =/= ?EMPTY ->
    case wocky_db:is_valid_id(UUID) of
        true -> ok;
        false -> {error, {400, ["Invalid UUID: ", UUID]}}
    end;
verify_user_fields(_) -> ok.

verify_avatar_field(#{avatar := Avatar,
                      server := Server}) ->
    case tros:parse_url(Avatar) of
        {ok, {Server, FileID}} -> verify_file_id(FileID);
        _ -> {error, {400, ["Invalid or non-local avatar URL: ", Avatar]}}
    end;
verify_avatar_field(_) -> ok.

verify_file_id(FileID) ->
    case wocky_db:is_valid_id(FileID) of
        true -> ok;
        false -> {error, {400, ["Invalid file ID in URL: ", FileID]}}
    end.

maybe_add_default_server(Fields = #{server := ?NOT_EMPTY}, _) -> Fields;
maybe_add_default_server(Fields, Server) -> Fields#{server => Server}.

authenticate(Fields = #{sessionID := SessionID}, _, _) ->
    case verify_session(Fields, SessionID) of
        true ->
            {true, session};
        false ->
            ok = lager:warning("Invalid registration session id '~s'",
                               [SessionID]),
            {false, 401, "Invalid sessionID"}
    end;
authenticate(#{phoneNumber := PhoneNumber} = Fields,
             AuthProviders, AuthBypassPrefixes) ->
    case has_any_prefix(PhoneNumber, AuthBypassPrefixes) of
        true ->
            {true, digits};
        false ->
            AuthProvider = maps:get('X-Auth-Service-Provider', Fields, <<>>),
            Auth = maps:get('X-Verify-Credentials-Authorization', Fields, <<>>),
            verify_digits_auth(Auth, PhoneNumber, AuthProvider, AuthProviders)
    end.

verify_digits_auth(Auth, PhoneNumber, AuthProvider, AuthProviders) ->
    case verify_auth(Auth, PhoneNumber, AuthProvider, AuthProviders) of
        true ->
            {true, digits};
        {false, Code, Error} ->
            ok = lager:warning("Failed Digits auth during registration: ~B ~s",
                               [Code, Error]),
            {false, Code, Error}
    end.

% Check that the auth server is one that we have configured as valid
verify_auth(Auth, PhoneNumber, AuthProvider, ValidProviders) ->
    case lists:member(binary_to_list(AuthProvider), ValidProviders) of
        true ->
            wocky_digits_auth:verify(Auth, PhoneNumber, AuthProvider);
        false ->
            ok = lager:warning("Invalid authentication provider '~s'",
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

    ok = lager:info("Registration complete. Replying with ~s", [Body]),
    {true, RD3, Ctx}.

create_update_error(E, RD, Ctx) ->
    RD2 = wrq:set_resp_header("content-type", "application/json", RD),
    RD3 = set_resp_body(409, atom_to_list(E), RD2),

    ok = lager:warning("Error during registration: ~p", [E]),
    {{halt, 409}, RD3, Ctx}.

create_internal_error(Class, Reason, Fields, RD, Ctx) ->
  ST = lager:pr_stacktrace(erlang:get_stacktrace(), {Class, Reason}),
  Msg = io_lib:format("Internal error registering user. ~p ~s", [Fields, ST]),
  ok = lager:error(Msg),
  {{halt, 500}, set_resp_body(500, Msg, RD), Ctx}.


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
                                       {error, iolist_to_binary(Error)}]}),
    wrq:set_resp_body(JSON, RD).

has_any_prefix(PhoneNumber, Prefixes) ->
    lists:any(fun(Prefix) -> has_prefix(PhoneNumber, Prefix) end,
              Prefixes).

has_prefix(Subject, Prefix) ->
    binary:longest_common_prefix([Subject, Prefix]) =:= byte_size(Prefix).
