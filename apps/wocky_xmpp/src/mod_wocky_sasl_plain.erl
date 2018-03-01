%%% @doc Module replacing cyrsasl_plain's SASL PLAIN handling to add
%%% our own custom OAuth 2 auth system
%%% See https://github.com/hippware/tr-wiki/wiki/User-registration-XMPP-protocol

-module(mod_wocky_sasl_plain).
-xep([{xep, 78}, {version, "2.5"}]).

-compile({parse_transform, do}).

-include("wocky.hrl").

-behaviour(gen_mod).
-behaviour(cyrsasl).

%% gen_mod handlers
-export([start/2, stop/1]).

%% cyrsasl handlers
-export([mech_new/2, mech_step/2]).

start(_Host, _Opts) ->
    % This *replaces* cyrsasl_plain's registration, since modules are
    % loaded/started after cyrsasl has registered its built-in modules.
    cyrsasl:register_mechanism(<<"PLAIN">>, ?MODULE, plain),
    ok.

stop(_Host) ->
    ok.

-spec mech_new(Host :: ejabberd:server(), Creds :: mongoose_credentials:t()) ->
    {ok, tuple()}.
mech_new(Host, Creds) ->
    cyrsasl_plain:mech_new(Host, Creds).

-spec mech_step(Creds :: mongoose_credentials:t(), ClientIn :: binary()) ->
    {ok, mongoose_credentials:t()} | {error, binary()}.
mech_step(_, <<0:8, "register", 0:8, "$J$", JSON/binary>>) ->
    case authenticate_user(JSON) of
        {ok, RegResult} ->
            make_auth_response(RegResult);
        {error, {Response, Text}} ->
            {error, {iolist_to_binary(Response), iolist_to_binary(Text)}}
    end;
mech_step(Creds, ClientIn) ->
    cyrsasl_plain:mech_step(Creds, ClientIn).

authenticate_user(JSON) ->
    do([error_m ||
        Elements <- decode_json(JSON),
        Provider <- get_required_field(Elements, <<"provider">>),
        Resource <- get_required_field(Elements, <<"resource">>),
        ProviderData <- get_provider_data(Elements),
        GetToken <- get_token(Elements),
        {User, IsNew} <- authenticate_user(Provider, ProviderData),
        {Token, Expiry} <- maybe_get_token(GetToken, User, Resource),
        {ok, {User, Provider, Token, Expiry, IsNew}}
       ]).

decode_json(Body) ->
    try mochijson2:decode(Body) of
        {struct, Elements} -> {ok, maps:from_list(Elements)}
    catch
        error:_ -> {error, {"malformed-request", "Could not parse JSON"}}
    end.

get_field(Field, Elements) ->
    case maps:find(Field, Elements) of
        {ok, Value} -> {ok, Value};
        error -> {ok, <<>>}
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

maybe_get_token(false, _, _) ->
    {ok, {undefined, undefined}};
maybe_get_token(true, #{id := UserID}, Resource) ->
    {ok, {Token, Expiry}} = ?wocky_account:assign_token(UserID, Resource),
    {ok, {Token, ?wocky_timestamp:to_string(Expiry)}}.

%% The digits provider exists only to support auth bypass
authenticate_user(<<"digits">>, Fields) ->
    Server = wocky_xmpp_app:server(),
    {ok, UserID} = get_field(<<"userID">>, Fields),
    {ok, PhoneNumber} = get_field(<<"phoneNumber">>, Fields),
    case ?wocky_account:authenticate_with_digits(Server, UserID, PhoneNumber) of
      {ok, Result} -> {ok, Result};
      {error, Error} -> {error, {"not-authorized", Error}}
    end;

authenticate_user(<<"firebase">>, #{<<"jwt">> := JWT}) ->
    Server = wocky_xmpp_app:server(),
    case ?wocky_account:authenticate_with_firebase(Server, JWT) of
        {ok, Result} -> {ok, Result};
        {error, Error} -> {error, {"not-authorized", Error}}
    end;

authenticate_user(P, _) -> {error, {"not-authorized",
                                    ["Unsupported provider: ", P]}}.

make_auth_response({User, Provider, Token, Expiry, IsNew}) ->
    #{id := UserID,
      server := Server,
      external_id := ExternalID,
      handle := Handle} = User,

    JSONFields = [{user, UserID},
                  {server, Server},
                  {handle, safe_handle(Handle)},
                  {provider, Provider},
                  {external_id, ExternalID},
                  {is_new, IsNew} |
                  maybe_token_fields(Token, Expiry)],
    JSON = mochijson2:encode({struct, JSONFields}),
    {error, {<<"redirect">>, JSON}}.

safe_handle(nil) -> <<>>;
safe_handle(H) -> H.

maybe_token_fields(undefined, _) -> [];
maybe_token_fields(Token, TokenExpiry) ->
   [{token, Token},
    {token_expiry, TokenExpiry}].
