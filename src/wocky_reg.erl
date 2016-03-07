%%% @copyright 2016+ Hippware, Inc.
%%% @doc Wocky user registration server
%%%
%%% This module implements the Wocky registration protocol described at
%%% https://github.com/hippware/tr-wiki/wiki/Registration-Protocol
-module(wocky_reg).

-export([start/1, stop/0]).

%% Webmachine callbacks
-export([init/1,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         malformed_request/2,
         forbidden/2,
         post_is_create/2,
         create_path/2,
         from_json/2,
         to_json/2
        ]).

-ifdef(TEST).
-export([verify_session/2,
         verify_auth/4
        ]).
-endif.

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-define(DEFAULT_PORT, 1096).
-define(MAX_REQ_LEN, 4096).

-record(state, {
          server :: binary(),
          auth_providers :: [string()],
          is_new = false :: boolean(),
          handle_set = false :: boolean(),
          phone_number_set = false :: boolean(),
          fields :: map(),           % Parsed request fields
          create_allowed = false :: boolean()
         }).

%%%===================================================================
%%% Top-level interface
%%%===================================================================

start(Opts) ->
    Port = proplists:get_value(port, Opts, ?DEFAULT_PORT),

    {ok, _} = application:ensure_all_started(webmachine),
    webmachine_mochiweb:start(
      [{name, wocky_reg},
       {port, Port},
       {dispatch, [{[], ?MODULE, Opts}]}]),
    ok.

stop() ->
    ok = webmachine_mochiweb:stop(wocky_reg_mochiweb),
    ok.

%%%===================================================================
%%% Webmachine callbacks
%%%===================================================================

init(Opts) ->
    AuthProviders = proplists:get_value(auth_providers, Opts),
    Server = proplists:get_value(server, Opts),
    {ok, #state{
            auth_providers = AuthProviders,
            server = Server
           }}.

allowed_methods(RD, Ctx) ->
    {['POST'], RD, Ctx}.

content_types_accepted(RD, Ctx) ->
    {[{"application/json", from_json}], RD, Ctx}.

content_types_provided(RD, Ctx) ->
    {[{"application/json", to_json}], RD, Ctx}.

malformed_request(RD, Ctx) ->
    try mochijson2:decode(wrq:req_body(RD)) of
        {struct, Elements} ->
            malformed_request(Elements, RD, Ctx)
    catch
        error:_ ->
            RD2 = set_resp_body(400, "Invalid JSON", RD),
            {true, RD2, Ctx}
    end.

forbidden(RD, Ctx = #state{fields = Fields,
                           auth_providers = AuthProviders}) ->
    case authenticate(Fields, AuthProviders) of
        {true, digits} ->
            {false, RD, Ctx#state{create_allowed = true}};
        {true, session} ->
            {false, RD, Ctx#state{fields = maps:remove(phoneNumber, Fields)}};
        {false, Code, Error} ->
            RD2 = set_resp_body(Code, Error, RD),
            {true, RD2, Ctx}
    end.

-spec from_json(#wm_reqdata{}, #state{}) ->
    {true, #wm_reqdata{}, #state{}}.
from_json(RD, Ctx = #state{create_allowed = true}) ->
    create_or_update_user(RD, Ctx);
from_json(RD, Ctx) ->
    find_and_update_user(RD, Ctx).

%%%===================================================================
%%% Request processing helper functions
%%%===================================================================

malformed_request(Elements, RD, Ctx = #state{server = Server}) ->
    Fields = map_keys_to_atoms(maps:from_list(Elements)),
    case verify_fields(Fields) of
        true ->
            Fields2 = maybe_add_default_server(Fields, Server),
            {false, RD, Ctx#state{fields = Fields2}};
        false ->
            RD2 = set_resp_body(400, "Missing required element(s)", RD),
            {true, RD2, Ctx}
    end.

verify_fields(Fields) ->
    verify_auth_fields(Fields) andalso
    verify_user_fields(Fields).

verify_auth_fields(#{sessionID := _}) -> true;
verify_auth_fields(#{'X-Auth-Service-Provider'            := _,
                     'X-Verify-Credentials-Authorization' := _,
                     phoneNumber                          := _
                    }) -> true;
verify_auth_fields(_) -> false.

verify_user_fields(#{uuid     := _,
                     resource := _
                    }) -> true;
verify_user_fields(#{userID   := _,
                     resource := _
                    }) -> true;
verify_user_fields(_) -> false.

maybe_add_default_server(Fields = #{server := _}, _) -> Fields;
maybe_add_default_server(Fields, Server) -> Fields#{server => Server}.

post_is_create(RD, Ctx) -> {true, RD, Ctx}.

create_path(RD, Ctx) -> {"", RD, Ctx}.

authenticate(
    #{
      'X-Auth-Service-Provider'            := AuthProvider,
      'X-Verify-Credentials-Authorization' := Auth,
      phoneNumber                          := PhoneNumber
     }, AuthProviders) ->
    case verify_auth(Auth, PhoneNumber, AuthProvider, AuthProviders) of
        true ->
            {true, digits};
        {false, Code, Error} ->
            {false, Code, Error}
    end;
authenticate(
    Fields = #{
      sessionID := SessionID
     }, _) ->
    case verify_session(Fields, SessionID) of
        true ->
            {true, session};
        false ->
            {false, 401, "Invalid sessionID"}
    end.

% Check that the auth server is one that we have configured as valid
verify_auth(Auth, PhoneNumber, AuthProvider, ValidProviders) ->
    case lists:member(binary_to_list(AuthProvider), ValidProviders) of
        true ->
            wocky_digits_auth:verify(Auth, PhoneNumber, AuthProvider);
        false ->
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

%% Check if the user handle exists; create it if it doesnt, possibly update
%% the user data otherwise
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

%% Check that the required fields are present, then create the user
create_user(RD, Ctx = #state{fields = Fields}) ->
    UUID = wocky_db_user:create_user(json_to_row(Fields)),
    finalize_changes(RD, Ctx#state{is_new = true,
                                   fields = Fields#{uuid => UUID}}).

find_and_update_user(RD, Ctx = #state{fields = #{uuid := _}}) ->
    update_user(RD, Ctx);
find_and_update_user(RD, Ctx = #state{fields = Fields
                                             = #{userID := AuthUser,
                                                 server := Server}}) ->
    UUID = wocky_db_user:get_user_by_auth_name(Server, AuthUser),
    update_user(RD, Ctx#state{fields = Fields#{uuid => UUID}}).

update_user(RD, Ctx = #state{fields = Fields}) ->
    wocky_db_user:update_user(json_to_row(Fields)),
    finalize_changes(RD, Ctx).

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
    {true, RD3, Ctx}.

% This function is required to keep webmachine happy (since it must be
% specified in content_types_provided) but is not actually called because
% the body is set in set_result, above.
to_json(RD, Ctx) ->
    {wrq:resp_body(RD), RD, Ctx}.

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
     {email,        email}
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

map_keys_to_atoms(Map) ->
    lists:foldl(fun(K, M) -> binary_key_to_atom(K, M) end,
                Map, maps:keys(Map)).

binary_key_to_atom(Key, Map) ->
    {ok, Val} = maps:find(Key, Map),
    maybe_add_as_atom(Key, Val, (maps:remove(Key, Map))).

maybe_add_as_atom(Key, Val, Map) ->
    try list_to_existing_atom(binary_to_list(Key)) of
        AtomKey -> Map#{AtomKey => Val}
    catch
        error:badarg -> Map
    end.

prepare_for_encoding(Fields = #{uuid := UUID}) ->
    Fields#{uuid => wocky_db_user:normalize_id(UUID)}.

set_resp_body(Code, Error, RD) ->
    JSON = mochijson2:encode({struct, [{code, Code},
                                       {error, list_to_binary(Error)}]}),
    wrq:set_resp_body(JSON, RD).
