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
         post_is_create/2,
         create_path/2,
         from_json/2,
         to_json/2
        ]).

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
          resource :: binary()
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
%       {ssl, true},
%       {ssl_opts, wocky_util:ssl_opts()},
       {dispatch, [{[], ?MODULE, Opts}]}]),
    ok.

stop() ->
    ok = webmachine_mochiweb:stop(wocky_reg),
    ok.

%%%===================================================================
%%% Webmachine callbacks
%%%===================================================================

init(Opts) ->
    AuthProviders = proplists:get_value(auth_providers, Opts),
    Server = proplists:get_value(server, Opts),
    {{trace, "/tmp"}, #state{
            auth_providers = AuthProviders,
            server = Server
           }}.

allowed_methods(RD, Ctx) ->
    {['POST'], RD, Ctx}.

content_types_accepted(RD, Ctx) ->
    {[{"application/json", from_json}], RD, Ctx}.

content_types_provided(RD, Ctx) ->
    {[{"application/json", to_json}], RD, Ctx}.

post_is_create(RD, Ctx) -> {true, RD, Ctx}.

create_path(RD, Ctx) -> {"", RD, Ctx}.

%%%===================================================================
%%% Request processing functions
%%%===================================================================

%% Pull out the JSON and convert it to an atom-keyed map
-spec from_json(#wm_reqdata{}, #state{}) ->
    {true | {halt, {non_neg_integer(), string()} | non_neg_integer()},
     #wm_reqdata{}, #state{}}.
from_json(RD, Ctx) ->
    {struct, Elements} = mochijson2:decode(wrq:req_body(RD)),
    Fields = map_keys_to_atoms(maps:from_list(Elements)),
    check_and_authenticate(Fields, RD, Ctx).

%% Check for required field being present and call the authentication function
check_and_authenticate(
  Fields = #{resource := Resource}, RD, Ctx) ->
    authenticate(Fields#{server => Ctx#state.server},
                 RD, Ctx#state{resource = Resource});
check_and_authenticate(_, RD, Ctx) ->
    missing_field(RD, Ctx).

authenticate(
    Fields = #{
      'X-Auth-Service-Provider'            := AuthProvider,
      'X-Verify-Credentials-Authorization' := Auth,
      phoneNumber                          := PhoneNumber
     }, RD, Ctx) ->
    case verify_auth(Auth, PhoneNumber, AuthProvider,
                     Ctx#state.auth_providers) of
        true ->
            maybe_create_user(Fields, RD, Ctx);
        {false, Code, Error} ->
            RD1 = wrq:set_resp_body(Error, RD),
            {{halt, Code}, RD1, Ctx}
    end;
authenticate(
    Fields = #{
      sessionID := SessionID
     }, RD, Ctx) ->
    case verify_session(Fields, SessionID) of
        true ->
            UpdateFields = maps:remove(phoneNumber, Fields),
            maybe_update_user(UpdateFields, RD, Ctx);
        false ->
            {{halt, {401, "Invalid sessionID"}}, RD, Ctx}
    end;
authenticate(_, RD, Ctx) ->
    missing_field(RD, Ctx).

verify_session(#{uuid := UUID, server := Server, resource := Resource},
               SessionID) ->
    wocky_db_user:check_token(UUID, Server, Resource, SessionID);
verify_session(Fields = #{userID := UserID, server := Server}, SessionID) ->
    case wocky_db_user:auth_user_user(Server, UserID) of
        not_found -> false;
        User -> verify_session(Fields#{uuid => User}, SessionID)
    end;
verify_session(_, _) ->
    false.

% Check that the auth server is one that we have configured as valid
verify_auth(Auth, PhoneNumber, AuthProvider, ValidProviders) ->
    case lists:member(binary_to_list(AuthProvider), ValidProviders) of
        true ->
            digits_auth:verify(Auth, PhoneNumber, AuthProvider);
        false ->
            {false, 401, "Invalid authentication provider"}
    end.

%% Check if the user handle exists; create it if it doesnt, possibly update
%% the user data otherwise
maybe_create_user(Fields = #{userID := AuthUser, server := Server}, RD, Ctx) ->
    case wocky_db_user:auth_user_user(Server, AuthUser) of
        not_found ->
            NewUser = wocky_db_user:create_id(),
            create_user(Fields#{uuid => NewUser}, RD, Ctx);
        ExistingUser ->
            maybe_update_user(Fields#{uuid => ExistingUser}, RD, Ctx)
    end.

%% Check that the required fields are present, then create the user
create_user(Fields, RD, Ctx) ->
    wocky_db_user:create_user(json_to_row(Fields)),
    finalise_changes(Fields, RD, Ctx#state{is_new = true}).

maybe_update_user(Fields = #{uuid := User, server := Server}, RD, Ctx) ->
    ExistingUser = row_to_json(wocky_db_user:get_user_data(Server, User)),
    check_update_user(ExistingUser, Fields, RD, Ctx);

maybe_update_user(Fields = #{userID := AuthUser, server := Server}, RD, Ctx) ->
    ExistingUser = row_to_json(wocky_db_user:auth_user(Server, AuthUser)),
    check_update_user(ExistingUser, Fields, RD, Ctx).

%% Update the user but only if the userID (auth_user) matches the one
%% currently in the DB
check_update_user(_ExistingUser = #{userID := ExistingAU, uuid := User},
                  NewUser       = #{userID := ExistingAU},
                  RD, Ctx) ->
    update_user(NewUser#{uuid => User}, RD, Ctx);

check_update_user(_ExistingUser = #{uuid := ExistingUUID},
                  NewUser       = #{uuid := ExistingUUID},
                  RD, Ctx) ->
    update_user(NewUser, RD, Ctx);

check_update_user(_, _, RD, Ctx) ->
    {{halt, {401, "User credentials do not match existing user"}}, RD, Ctx}.

update_user(Fields, RD, Ctx) ->
    wocky_db_user:update_user(json_to_row(Fields)),
    finalise_changes(Fields, RD, Ctx).

finalise_changes(Fields = #{uuid := UUID}, RD, Ctx) ->
    Ctx2 = maybe_update_handle(Fields, Ctx),
    Ctx3 = maybe_update_phone_number(Fields, Ctx2),
    set_result(UUID, RD, Ctx3).

maybe_update_handle(#{uuid := User, handle := Handle},
                    Ctx = #state{server = Server}) ->
    Set = wocky_db_user:maybe_set_handle(User, Server, Handle),
    Ctx#state{handle_set = Set};
maybe_update_handle(_, Ctx) -> Ctx.

maybe_update_phone_number(#{uuid := User, phoneNumber := PhoneNumber},
                         Ctx = #state{server = Server}) ->
    Set = wocky_db_user:set_phone_number(User, Server, PhoneNumber),
    Ctx#state{phone_number_set = Set};
maybe_update_phone_number(_, Ctx) -> Ctx.

set_result(UUID, RD, Ctx = #state{server = Server,
                                  is_new = IsNew,
                                  phone_number_set = PhoneNumberSet,
                                  handle_set = HandleSet,
                                  resource = Resource}) ->
    Fields = row_to_json(wocky_db_user:get_user_data(Server, UUID)),
    JSONFields = prepare_for_encoding(Fields),
    {ok, Token} = wocky_db_user:assign_token(UUID, Server, Resource),
    Ret = [{sessionID, Token}, {isNew, IsNew},
           {phoneNumberSet, PhoneNumberSet}, {handleSet, HandleSet},
           {resource, Resource} | maps:to_list(JSONFields)],
    Body = mochijson2:encode({struct, Ret}),
    RD2 = wrq:set_resp_header("content-type", "application/json", RD),
    RD3 = wrq:set_resp_body(Body, RD2),
    {true, RD3, Ctx}.

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
     {emailAddress, email}
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

missing_field(RD, Ctx) ->
    {{halt, {400, "Missing mandatory field"}}, RD, Ctx}.

