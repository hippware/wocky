%%% @copyright 2016+ Hippware, Inc.
%%% @doc Wocky user registration server
%%%
%%% This module implements verification against Tiwtter Digits servers.
%%% It checks the authorisation strings as well as verifying that the phone
%%% number returned matches the one the user supplied.
-module(wocky_digits_auth).

-export([verify/1, verify/3]).

-compile({parse_transform, do}).

-spec verify(map()) -> {ok, {binary(), binary()}} |
                       {error, {non_neg_integer(), iolist()}}.
verify(Fields) ->
    do([error_m ||
        UserID <- get_required_field(<<"userID">>, Fields),
        PhoneNumber <- get_required_field(<<"phoneNumber">>, Fields),
        AuthProvider <- get_required_field(
                          <<"X-Auth-Service-Provider">>, Fields),
        Auth <- get_required_field(
                  <<"X-Verify-Credentials-Authorization">>, Fields),
        check_valid_provider(AuthProvider),
        verify(Auth, PhoneNumber, AuthProvider),
        {ok, {UserID, PhoneNumber}}
       ]).

check_valid_provider(AuthProvider) ->
    Providers =
    ejabberd_config:get_local_option_or_default(wocky_sasl_auth_providers, []),
    case lists:member({digits, binary_to_list(AuthProvider)}, Providers) of
        true -> ok;
        false -> {error, {401, "Invalid authentication provider"}}
    end.

-spec verify(binary(), binary(), binary()) ->
    ok | {error, {non_neg_integer(), iolist()}}.
verify(Auth, PhoneNumber, AuthProvider) ->
    case has_bypass_prefix(PhoneNumber) of
        true -> ok;
        false -> do_digits_verify(Auth, PhoneNumber, AuthProvider)
    end.

do_digits_verify(Auth, PhoneNumber, AuthProvider) ->
    case httpc:request(get, {binary_to_list(AuthProvider),
                             [{"Authorization", binary_to_list(Auth)}]},
                             [], [{full_result, false}]) of
        {ok, {200, Body}} ->
            verify_phone_number(PhoneNumber, Body);
        {ok, {Code, Body}} ->
            {error, {401, ["Digits validation error: ",
                           integer_to_list(Code), " ", Body]}};
        {error, Reason} ->
            {error, {500, ["Digits error: ", io_lib:format("~p", [Reason])]}}
    end.

verify_phone_number(PhoneNumber, Body) ->
    {struct, Elements} = mochijson2:decode(Body),
    case proplists:get_value(<<"phoneNumber">>, Elements) of
        PhoneNumber -> ok;
        undefined -> {error, {401, "No phone number returned by Digits"}};
        OtherNumber -> {error, {401, ["Supplied phone number ", PhoneNumber,
                                      " did not match Digits-provided one ",
                                      OtherNumber]}}
    end.

get_required_field(Field, Elements) ->
    case maps:find(Field, Elements) of
        {ok, Value} -> {ok, Value};
        error -> {error, {401, ["Missing '", Field, "' field"]}}
    end.

has_bypass_prefix(PhoneNumber) ->
    Prefixes = ejabberd_config:get_local_option_or_default(
                 wocky_sasl_bypass_prefixes, []),
    lists:any(fun(Prefix) -> has_prefix(PhoneNumber, Prefix) end,
              Prefixes).

has_prefix(Subject, Prefix) ->
    binary:longest_common_prefix([Subject, Prefix]) =:= byte_size(Prefix).

