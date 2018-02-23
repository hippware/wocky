-module(wocky_reg).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-include("wocky.hrl").
-include("wocky_reg.hrl").

-export([register_user/1]).

-type reg_result() :: #reg_result{}.

-define(DEFAULT_HS_PREPOP_DAYS, 28).
-define(DEFAULT_HS_MIN_PREPOP, 10).

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
        {UserID, Server, IsNew} <- create_or_update_user(Provider,
                                                         ExternalID,
                                                         PhoneNumber),

        {ok, IsNew andalso prepopulate_user(UserID)},

        {Token, Expiry} <- maybe_get_token(GetToken, UserID, Resource),
        {ok, #reg_result{
                user = UserID,
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

check_provider_auth(<<"firebase">>, #{<<"jwt">> := JWT}) ->
    case ?wocky_firebase:verify(JWT) of
        {ok, Result} -> {ok, Result};
        {error, Error} -> {error, {"not-authorized", Error}}
    end;

check_provider_auth(P, _) -> {error, {"not-authorized",
                                      ["Unsupported provider: ", P]}}.

create_or_update_user(Provider, ExternalId, PhoneNumber) ->
    ?wocky_user:register_external(
       wocky_xmpp_app:server(), Provider, ExternalId, PhoneNumber).

maybe_get_token(false, _, _) ->
    {ok, {undefined, undefined}};
maybe_get_token(true, User, Resource) ->
    {ok, {Token, Expiry}} = ?wocky_token:assign(User, Resource),
    {ok, {Token, ?wocky_timestamp:to_string(Expiry)}}.

prepopulate_user(UserID) ->
    set_initial_contacts(UserID),
    prepopulate_home_stream(UserID).

set_initial_contacts(UserID) ->
    InitialFollowees = ?wocky_initial_contact:get(),
    lists:foreach(set_initial_contact(UserID, _), InitialFollowees).

set_initial_contact(UserID, #{user := User, type := followee}) ->
    set_initial_contact(UserID, User, to, from);
set_initial_contact(UserID, #{user := User, type := follower}) ->
    set_initial_contact(UserID, User, from, to);
set_initial_contact(UserID, #{user := User, type := friend}) ->
    set_initial_contact(UserID, User, both, both).

set_initial_contact(UserID, #{id := FolloweeID, handle := Handle},
                    USub, FSub) ->
    UserContact = #{user_id => UserID,
                    contact_id => FolloweeID,
                    name => Handle,
                    ask => none,
                    subscription => USub,
                    groups => [<<"__welcome__">>, <<"__new__">>]
                   },

    InitContact = #{user_id => FolloweeID,
                    contact_id => UserID,
                    name => <<>>,
                    ask => none,
                    subscription => FSub,
                    groups => [<<"__welcomed__">>, <<"__new__">>]
                   },

    ?wocky_roster_item:put(UserContact),
    ?wocky_roster_item:put(InitContact).

prepopulate_home_stream(UserID) ->
    prepopulate_from_user(UserID, ?wocky_user_hs_prepop:user()).

prepopulate_from_user(_, nil) -> ok;
prepopulate_from_user(UserID, #{id := SourceID}) ->
    Period = ?confex:get_env(wocky_xmpp, hs_prepopulation_days,
                         ?DEFAULT_HS_PREPOP_DAYS),
    Min = ?confex:get_env(wocky_xmpp, hs_prepopulation_min,
                          ?DEFAULT_HS_MIN_PREPOP),
    ?wocky_home_stream_item:prepopulate_from(
       UserID, SourceID, ?duration:from_days(Period), Min).
