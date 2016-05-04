-module(wocky_reg).

-include("wocky.hrl").
-include("wocky_reg.hrl").

-export([register_user/1]).

-type reg_result() :: #reg_result{}.

-compile({parse_transform, do}).

-spec register_user(binary()) ->
    {ok, reg_result()} | {error, {string(), string()}}.
register_user(JSON) ->
    do([error_m ||
        Elements <- decode_json(JSON),
        Provider <- get_required_field(Elements, <<"provider">>),
        Resource <- get_required_field(Elements, <<"resource">>),
        ProviderData <- get_provider_data(Elements),
        GetToken <- get_token(Elements),
        {ExternalID, PhoneNumber} <- check_provider_auth(Provider,
                                                         ProviderData),
        {User, Server, IsNew} <- create_or_update_user(ExternalID,
                                                       PhoneNumber),
        Token <- maybe_get_token(GetToken, User, Server, Resource),
        {ok, #reg_result{
                user = User,
                server = Server,
                provider = Provider,
                is_new = IsNew,
                token = Token,
                token_expiry = ?TOKEN_EXPIRE,
                external_id = ExternalID}}
       ]).

decode_json(Body) ->
    try mochijson2:decode(Body) of
        {struct, Elements} -> {ok, maps:from_list(Elements)}
    catch
        error:_ -> {error, {"malformed-request", "Could not parse JSON"}}
    end.

get_required_field(Elements, Field) ->
    case maps:find(Field, Elements) of
        {ok, Value} -> {ok, Value};
        error -> {error, {"malformed-request",
                          ["Missing ", Field, " field"]}}
    end.

get_token(Elements) ->
    {ok, maps:get(<<"token">>, Elements, false)}.

get_provider_data(Elements) ->
    case maps:get(<<"provider_data">>, Elements, {struct, []}) of
        {struct, ProviderData} -> {ok, maps:from_list(ProviderData)};
        _ -> {error, {"malformed-request", "Invalid provider_data"}}
    end.

check_provider_auth("digits", ProviderData) ->
    case wocky_digits_auth:verify(ProviderData) of
        {ok, Result} -> {ok, Result};
        {error, {500, Error}} -> {error, {"temporary-auth-failure", Error}};
        {error, {_, Error}} -> {error, {"not-authorized", Error}}
    end;

check_provider_auth(P, _) -> {error, {"not-authorized",
                                      ["Unsupported provider: ", P]}}.

create_or_update_user(ExternalID, PhoneNumber) ->
    case wocky_db_user:get_user_by_external_id(
           wocky_app:server(), ExternalID) of
        not_found ->
            create_user(ExternalID, PhoneNumber);
        User ->
            update_user(User, PhoneNumber)
    end.

update_user(User, PhoneNumber) ->
    Server = case wocky_db_user:get_user_data(User, wocky_app:server()) of
        #{phone_number := PhoneNumber, server := Server_} ->
            Server_;
        #{server := Server_} ->
            ok = wocky_db_user:set_phone_number(User, Server_, PhoneNumber),
            Server_
    end,
    {ok, {User, Server, false}}.

create_user(ExternalID, PhoneNumber) ->
    % TODO: This is where the code to assign a user to a particular server
    % will go:
    Server = wocky_app:server(),
    ct:log("BJD Server = ~p", [Server]),

    User = wocky_db_user:create_user(#{external_id => ExternalID, server => Server}),
    ok = wocky_db_user:set_phone_number(User, Server, PhoneNumber),
    {ok, {User, Server, true}}.

maybe_get_token(false, _, _, _) ->
    {ok, undefined};
maybe_get_token(true, User, Server, Resource) ->
    {ok, Token} = wocky_db_user:assign_token(User, Server, Resource),
    {ok, Token}.
