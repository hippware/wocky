%%% @copyright 2016+ Hippware, Inc.
%%% @doc Helper functions for Wocky integration test suites
-module(test_helper).

-export([ensure_wocky_is_running/0]).

-export([expect_iq_success/2, expect_iq_error/2]).

ensure_wocky_is_running() ->
    case net_kernel:start(['mongooseim@localhost', longnames]) of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end,

    Applications = application:which_applications(),
    case app_is_running(Applications, wocky) of
        true -> ok;
        false -> ok = wocky_app:start("test")
    end,
    case app_is_running(Applications, ejabberd) of
        true -> ok;
        false -> wocky_app:start_ejabberd("../../../../etc")
    end.

app_is_running(Applications, Name) ->
    case lists:keysearch(Name, 1, Applications) of
        {value, _} -> true;
        false -> false
    end.


expect_iq_success(Stanza, User) ->
    expect_something(Stanza, User, is_iq_result).

expect_iq_error(Stanza, User) ->
    expect_something(Stanza, User, is_iq_error).

expect_something(Stanza, User, Expect) ->
    FinalStanza = add_to(Stanza, User),
    ct:log("Sending stanza: ~p", [FinalStanza]),
    ResultStanza = escalus:send_and_wait(User, FinalStanza),
    ct:log("Result stanza: ~p", [ResultStanza]),
    escalus:assert(Expect, ResultStanza),
    ResultStanza.

add_to(Stanza, User) ->
    escalus_stanza:to(Stanza,
      escalus_client:server(User)).
