%%% @copyright 2016+ Hippware, Inc.
%%% @doc Helper functions for Wocky integration test suites
-module(test_helper).

-export([start_ejabberd/0, stop_ejabberd/0]).

-export([expect_iq_success/2, expect_iq_error/2]).

start_ejabberd() ->
    case net_kernel:start(['mongooseim@localhost', longnames]) of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end,

    ok = wocky_app:start("test"),
    wocky_app:start_ejabberd("../../../../etc").

stop_ejabberd() ->
    wocky_app:stop().

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

