%%% @copyright 2016+ Hippware, Inc.
%%% @doc Wocky user registration server
%%%
%%% This module implements verification against Tiwtter Digits servers.
%%% It checks the authorisation strings as well as verifying that the phone
%%% number returned matches the one the user supplied.
-module(digits_auth).

-export([verify/3]).

-spec verify(binary(), binary(), binary()) ->
    true | {false, non_neg_integer(), iolist()}.
verify(Auth, PhoneNumber, AuthProvider) ->
    case httpc:request(get, {binary_to_list(AuthProvider),
                             [{"Authorization", binary_to_list(Auth)}]},
                             [], [{full_result, false}]) of
        {ok, {200, Body}} ->
            verify_phone_number(PhoneNumber, Body);
        {ok, {Code, Body}} ->
            {false, 401, [integer_to_list(Code), " ", Body]};
        {error, Reason} ->
            {false, 500, [io_lib:format("~p", [Reason])]}
    end.

verify_phone_number(PhoneNumber, Body) ->
    {struct, Elements} = mochijson2:decode(Body),
    case proplists:get_value(<<"phone_number">>, Elements) of
        PhoneNumber -> true;
        undefined -> {false, 500, "No phone number returned by Digits"};
        OtherNumber -> {false, 401, ["Supplied phone number ", PhoneNumber,
                                     " did not match Digits-supplied one ",
                                     OtherNumber]}
    end.
