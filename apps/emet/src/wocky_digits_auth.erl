%%% @copyright 2016+ Hippware, Inc.
%%% @doc Wocky user registration server
%%%
%%% This module implements verification against Tiwtter Digits servers.
%%% It checks the authorisation strings as well as verifying that the phone
%%% number returned matches the one the user supplied.
-module(wocky_digits_auth).

-export([verify/1]).

-compile({parse_transform, do}).

-spec verify(map()) -> {ok, {binary(), binary()}} |
                       {error, {non_neg_integer(), iolist()}}.
verify(Fields) ->
    do([error_m ||
        SuppliedPhoneNumber <- get_optional_field(
                                 <<"phoneNumber">>, Fields, <<>>),
        SuppliedUserID <- get_optional_field(
                                 <<"userID">>, Fields, <<>>),
        AuthProvider <- get_required_field(
                          <<"X-Auth-Service-Provider">>, Fields),
        Auth <- get_required_field(
                  <<"X-Verify-Credentials-Authorization">>, Fields),
        check_valid_provider(AuthProvider),
        {UserID, PhoneNumber} <- verify(
                                   Auth, AuthProvider,
                                   SuppliedUserID, SuppliedPhoneNumber),
        {ok, {UserID, PhoneNumber}}
       ]).

check_valid_provider(AuthProvider) ->
    Providers =
    ejabberd_config:get_local_option_or_default(wocky_sasl_auth_providers, []),
    case lists:member({digits, binary_to_list(AuthProvider)}, Providers) of
        true -> ok;
        false -> {error, {401, "Invalid authentication provider"}}
    end.

-spec verify(binary(), binary(), binary(), binary()) ->
    {ok, {binary(), binary()}} | {error, {non_neg_integer(), iolist()}}.
verify(Auth, AuthProvider, UserID, PhoneNumber) ->
    case has_bypass_prefix(PhoneNumber) of
        true -> {ok, {UserID, PhoneNumber}};
        false -> do_digits_verify(Auth, AuthProvider)
    end.

do_digits_verify(Auth, AuthProvider) ->
    case httpc:request(get, {binary_to_list(AuthProvider),
                             [{"Authorization", binary_to_list(Auth)}]},
                             [], [{full_result, false}]) of
        {ok, {200, Body}} ->
            ok = lager:debug("Received Digits response: ~s", [Body]),
            get_userid_and_phonenumber(Body);
        {ok, {Code, Body}} ->
            {error, {401, ["Digits validation error: ",
                           integer_to_list(Code), " ", Body]}};
        {error, Reason} ->
            {error, {500, ["Digits error: ", io_lib:format("~p", [Reason])]}}
    end.

get_userid_and_phonenumber(Body) ->
    {struct, Elements} = mochijson2:decode(Body),
    do([error_m ||
        PhoneNumber <- get_digits_value(<<"phone_number">>, Elements),
        UserID <- get_digits_value(<<"id_str">>, Elements),
        {ok, {UserID, PhoneNumber}}
       ]).

get_digits_value(Key, Elements) ->
    case proplists:get_value(Key, Elements) of
        undefined -> {error, {500, ["No ", Key, " returned by Digits"]}};
        Value -> {ok, Value}
    end.

get_required_field(Field, Elements) ->
    case maps:find(Field, Elements) of
        {ok, Value} -> {ok, Value};
        error -> {error, {401, ["Missing '", Field, "' field"]}}
    end.

get_optional_field(Field, Elements, Default) ->
    case maps:find(Field, Elements) of
        {ok, Value} -> {ok, Value};
        error -> {ok, Default}
    end.

has_bypass_prefix(PhoneNumber) ->
    Prefixes = ejabberd_config:get_local_option_or_default(
                 wocky_sasl_bypass_prefixes, []),
    lists:any(fun(Prefix) -> has_prefix(PhoneNumber, Prefix) end,
              Prefixes).

has_prefix(Subject, Prefix) ->
    binary:longest_common_prefix([Subject, Prefix]) =:= byte_size(Prefix).
