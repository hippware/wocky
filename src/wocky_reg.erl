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
        {Token, Expiry} <- maybe_get_token(GetToken, User, Server, Resource),
        {ok, #reg_result{
                user = User,
                server = Server,
                provider = Provider,
                is_new = IsNew,
                token = Token,
                token_expiry = Expiry,
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

check_provider_auth(<<"digits">>, ProviderData) ->
    case wocky_digits_auth:verify(ProviderData) of
        {ok, Result} -> {ok, Result};
        {error, {500, Error}} -> {error, {"temporary-auth-failure", Error}};
        {error, {_, Error}} -> {error, {"not-authorized", Error}}
    end;

check_provider_auth(P, _) -> {error, {"not-authorized",
                                      ["Unsupported provider: ", P]}}.

create_or_update_user(ExternalId, PhoneNumber) ->
    ?wocky_user:register(wocky_app:server(), ExternalId, PhoneNumber).

maybe_get_token(false, _, _, _) ->
    {ok, {undefined, undefined}};
maybe_get_token(true, User, Server, Resource) ->
    {ok, {Token, Expiry}} = ?wocky_user_token:assign(User, Server, Resource),
    {ok, {Token, wocky_db:timestamp_to_string(Expiry)}}.
