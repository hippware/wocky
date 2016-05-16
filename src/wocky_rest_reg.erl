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
    Server = proplists:get_value(server, Opts),
    AuthProviders = proplists:get_value(auth_providers, Opts),
    AuthBypassPrefixes = get_auth_bypass_prefixes(Opts),
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
            {false, RD, Ctx#state{fields = maps:remove(phone_number, Fields)}};
        {false, Code, Error} ->
            RD2 = set_resp_body(Code, Error, RD),
            {true, RD2, Ctx}
    end.

post_is_create(RD, Ctx) -> {true, RD, Ctx}.

create_path(RD, Ctx) -> {"", RD, Ctx}.

-spec from_json(#wm_reqdata{}, #state{}) ->
    {true, #wm_reqdata{}, #state{}}.
from_json(RD, Ctx = #state{create_allowed = Create, fields = Fields}) ->
    try
      case Create of
        true  -> create_or_update_user(RD, Ctx);
        false -> find_and_update_user(RD, Ctx)
      end
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
      external_id                          => ?EMPTY,
      user                                 => ?EMPTY,
      handle                               => ?EMPTY,
      first_name                           => ?EMPTY,
      last_name                            => ?EMPTY,
      resource                             => ?EMPTY,
      phone_number                         => ?EMPTY,
      email                                => ?EMPTY,
      token                                => ?EMPTY,
      'X-Auth-Service-Provider'            => undefined,
      'X-Verify-Credentials-Authorization' => undefined
     },
    maps:merge(Defaults, Fields).

strip_empty_fields(Fields) ->
  {ok, maps:filter(fun (_, <<>>) -> false;
                       (_, undefined) -> false;
                       (_, _) -> true end,
                   Fields)}.

verify_auth_fields(#{phone_number := ?EMPTY}) ->
    {error, {400, "Missing or empty phone_numbeer"}};
verify_auth_fields(#{'X-Auth-Service-Provider'            := X1,
                     'X-Verify-Credentials-Authorization' := X2,
                     token                                := ?EMPTY})
  when X1 =:= undefined orelse X2 =:= undefined ->
    {error, {400, "The Digits fields or token (or both) must be supplied"}};
verify_auth_fields(_) -> ok.

verify_user_fields(#{external_id := ?EMPTY, user := ?EMPTY}) ->
    {error, {400, "external_id or user (or both) must be supplied"}};
verify_user_fields(#{resource := ?EMPTY}) ->
    {error, {400, "resource must be supplied"}};
verify_user_fields(_) -> ok.

verify_avatar_field(#{avatar := Avatar,
                      server := Server}) ->
    case tros:parse_url(Avatar) of
        {ok, {Server, _FileID}} -> ok;
        _ -> {error, {400, ["Invalid or non-local avatar URL: ", Avatar]}}
    end;
verify_avatar_field(_) -> ok.

maybe_add_default_server(Fields = #{server := ?NOT_EMPTY}, _) -> Fields;
maybe_add_default_server(Fields, Server) -> Fields#{server => Server}.

authenticate(Fields = #{token := Token}, _, _) ->
    case verify_session(Fields, Token) of
        true ->
            {true, session};
        false ->
            ok = lager:warning("Invalid registration token '~s'",
                               [Token]),
            {false, 401, "Invalid token"}
    end;
authenticate(#{phone_number := PhoneNumber} = Fields,
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
        ok ->
            {true, digits};
        {error, {Code, Error}} ->
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
            {error, {401, "Invalid authentication provider"}}
    end.

verify_session(#{user := UUID, server := Server, resource := Resource},
               SessionID) ->
    wocky_db_user:check_token(UUID, Server, Resource, SessionID);
verify_session(Fields = #{external_id := ExternalID, server := Server},
               SessionID) ->
    case wocky_db_user:get_user_by_external_id(Server, ExternalID) of
        not_found ->
            false;
        User ->
            verify_session(Fields#{user => User}, SessionID)
    end.

create_or_update_user(RD, Ctx = #state{fields = Fields
                                              = #{external_id := ExternalID,
                                                  server := Server}}) ->
    case wocky_db_user:get_user_by_external_id(Server, ExternalID) of
        not_found ->
            create_user(RD, Ctx);
        ExistingUser ->
            find_and_update_user(RD,
                                 Ctx#state{fields =
                                           Fields#{user => ExistingUser}})
    end.

create_user(RD, Ctx = #state{fields = Fields}) ->
    case wocky_db_user:create_user(json_to_row(Fields)) of
        {error, E} -> create_update_error(E, RD, Ctx);
        UUID ->
            finalize_changes(RD, Ctx#state{is_new = true,
                                           fields = Fields#{user => UUID}})
    end.

find_and_update_user(RD, Ctx = #state{fields = #{user := _}}) ->
    update_user(RD, Ctx);
find_and_update_user(RD, Ctx = #state{fields = Fields
                                             = #{external_id := ExternalID,
                                                 server := Server}}) ->
    UUID = wocky_db_user:get_user_by_external_id(Server, ExternalID),
    update_user(RD, Ctx#state{fields = Fields#{user => UUID}}).

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
                                 fields = #{user := User,
                                            handle := Handle}}) ->
    Set = wocky_db_user:maybe_set_handle(User, Server, Handle),
    Ctx#state{handle_set = Set};
maybe_update_handle(Ctx) -> Ctx.

maybe_update_phone_number(Ctx = #state{server = Server,
                                       fields = #{user := User,
                                                  phone_number
                                                  := PhoneNumber}}) ->
    wocky_db_user:set_phone_number(User, Server, PhoneNumber),
    Ctx#state{phone_number_set = true};
maybe_update_phone_number(Ctx) -> Ctx.

set_result(RD, Ctx = #state{server = Server,
                            is_new = IsNew,
                            phone_number_set = PhoneNumberSet,
                            handle_set = HandleSet,
                            fields = #{user := UUID,
                                       resource := Resource}}) ->
    Fields = wocky_db:drop_nulls(
               wocky_db_user:get_user_data(UUID, Server)),
    {ok, Token, _} = wocky_db_user:assign_token(UUID, Server, Resource),
    Ret = [{token, Token}, {is_new, IsNew},
           {phone_number_set, PhoneNumberSet}, {handle_set, HandleSet},
           {resource, Resource} | maps:to_list(Fields)],
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

get_auth_bypass_prefixes(Opts) ->
  case wocky_app:is_testing() of
    true  -> proplists:get_value(auth_bypass_prefixes, Opts, []);
    false -> []
  end.

set_resp_body(Code, Error, RD) ->
    JSON = mochijson2:encode({struct, [{code, Code},
                                       {error, iolist_to_binary(Error)}]}),
    wrq:set_resp_body(JSON, RD).

has_any_prefix(PhoneNumber, Prefixes) ->
    lists:any(fun(Prefix) -> has_prefix(PhoneNumber, Prefix) end,
              Prefixes).

has_prefix(Subject, Prefix) ->
    binary:longest_common_prefix([Subject, Prefix]) =:= byte_size(Prefix).

json_to_row(JSONFields) ->
    maps:with(wocky_db:table_columns(user), JSONFields).
