%%==============================================================================
%% Copyright 2012 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
-module(mam_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml_stream.hrl").

-include("wocky_db_seed.hrl").

-compile({parse_transform, fun_chain}).

-define(ASSERT_EQUAL(E, V), (
    [ct:fail("ASSERT EQUAL~n\tExpected ~p~n\tValue ~p~n", [(E), (V)])
     || (E) =/= (V)]
    )).

-define(BALICE, nick_to_bjid(alice, Config)).
-define(BBOB, nick_to_bjid(bob, Config)).
-define(BCAROL, nick_to_bjid(carol, Config)).

-record(rsm_in, {
        max         :: non_neg_integer() | undefined,
        direction   :: before | 'after' | undefined,
        id          :: binary() | undefined,
        index       :: non_neg_integer() | undefined,
        after_id    :: binary() | undefined,
        before_id   :: binary() | undefined,
        from_id     :: binary() | undefined,
        to_id       :: binary() | undefined,
        reverse = false :: boolean(),
        simple = false :: boolean(),
        opt_count = false :: boolean()
        }).

-record(forwarded_message, {
    from           :: binary() | undefined,
    to             :: binary() | undefined,
    result_queryid :: binary() | undefined,
    result_id      :: binary() | undefined,
    delay_from     :: binary() | undefined,
    delay_stamp    :: binary() | undefined,
    message_to     :: binary() | undefined,
    message_type   :: binary() | undefined,
    message_body   :: binary() | undefined
}).

-record(result_iq, {
    from            :: binary(),
    to              :: binary(),
    id              :: binary(),
    first           :: binary() | undefined,
    first_index     :: non_neg_integer() | undefined,
    last            :: binary() | undefined,
    count           :: non_neg_integer()
}).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

host() ->
    <<"localhost">>.

all() ->
    [
     {group, mam},
     {group, mam_04},
     {group, bootstrapped},
     {group, rsm},
     {group, rsm_04}
    ].

groups() ->
    [{mam, [], mam_cases()},
     {mam_04, [], mam_cases()},
     {bootstrapped, [], bootstrapped_cases()},
     {rsm, [], rsm_cases()},
     {rsm_04, [], rsm_cases()}
    ].

bootstrapped_cases() ->
      [querying_for_all_messages_with_jid,
       querying_for_all_messages_with_no_jid
      ].

mam_cases() ->
    [
     mam_service_discovery,
     simple_archive_request,
     range_archive_request,
     range_archive_request_not_empty,
     limit_archive_request,
     iq_spoofing
     ].

rsm_cases() ->
      [pagination_first5,
       pagination_last5,
       pagination_before10,
       pagination_after10,
       pagination_empty_rset,
       %% Border cases
       pagination_last_after_id5,
       pagination_last_after_id5_before_id11,
       %% Simple cases
       pagination_simple_before10,
       %% opt_count cases
       pagination_first5_opt_count,
       pagination_last5_opt_count,
       pagination_offset5_opt_count,
       %% opt_count cases with all messages on the page
       pagination_first25_opt_count_all,
       pagination_last25_opt_count_all,
       pagination_offset5_opt_count_all,
       %% Reversed result list
       reverse
      ].

suite() ->
    escalus:suite().

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    wocky_db:clear_user_tables(?LOCAL_CONTEXT),
    wocky_db:clear_tables(?LOCAL_CONTEXT, [message_archive]),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

user_names() ->
    [alice, bob, carol].

create_users(Config) ->
    escalus:create_users(Config, escalus:get_users(user_names())).

delete_users(Config) ->
    escalus:delete_users(Config, escalus:get_users(user_names())).

init_per_group(Group, ConfigIn) ->
    ConfigOut = fun_chain:first(ConfigIn,
        init_modules(),
        create_users(),
        test_helper:make_everyone_friends(escalus:get_users(user_names()))
    ),
    init_state(Group, ConfigOut).

end_per_group(Group, Config) ->
    delete_users(Config),
    end_modules(Group, Config).

init_modules(Config) ->
    [init_module(host(), M, []) || M <- mam_modules()],
    Config.

end_modules(_, Config) ->
    [stop_module(host(), M) || M <- mam_modules()],
    Config.

mam_modules() ->
    [
     mod_mam,
     mod_wocky_mam
    ].

init_state(rsm, Config) ->
    send_rsm_messages(clean_archives(Config));
init_state(rsm_04, Config) ->
    Config1 = [{props, mam04_props()} | Config],
    send_rsm_messages(clean_archives(Config1));
init_state(mam_04, Config) ->
    Config1 = [{props, mam04_props()} | Config],
    clean_archives(Config1);
init_state(_, Config) ->
    clean_archives(Config).

mam04_props() ->
    [{data_form, true},                 %% send data forms
     {result_format, iq_fin},           %% RSM is inside iq with <fin/> inside
     {mam_ns, mam_ns_binary_v04()}].

mam_ns_binary_v04() -> <<"urn:xmpp:mam:1">>.

init_per_testcase(C=querying_for_all_messages_with_jid, Config) ->
    escalus:init_per_testcase(C,
        bootstrap_archive(clean_archives(Config)));
init_per_testcase(C=querying_for_all_messages_with_no_jid, Config) ->
    escalus:init_per_testcase(C,
        bootstrap_archive(clean_archives(Config)));
init_per_testcase(C=range_archive_request_not_empty, Config) ->
    escalus:init_per_testcase(C,
        bootstrap_archive(clean_archives(Config)));
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

init_module(Host, Mod, Args) ->
    stop_module(Host, Mod),
    ok = start_module(Host, Mod, Args).

is_loaded_module(Host, Mod) ->
    rpc_apply(gen_mod, is_loaded, [Host, Mod]).

start_module(Host, Mod, Args) ->
    rpc_apply(gen_mod, start_module, [Host, Mod, Args]).

stop_module(Host, Mod) ->
    case is_loaded_module(Host, Mod) of
        non_existing -> ok;
        false        -> ok;
        true         -> just_stop_module(Host, Mod)
    end.

just_stop_module(Host, Mod) ->
    {atomic, ok} = rpc_apply(gen_mod, stop_module, [Host, Mod]),
    ok.

rpc_apply(M, F, Args) ->
    case rpc_call(M, F, Args) of
    {badrpc, Reason} ->
        ct:fail("~p:~p/~p with arguments ~w fails with reason ~p.",
                [M, F, length(Args), Args, Reason]);
    Result ->
        Result
    end.

rpc_call(M, F, A) ->
    Node = escalus_ct:get_config(ejabberd_node),
    Cookie = escalus_ct:get_config(ejabberd_cookie),
    escalus_ct:rpc_call(Node, M, F, A, 10000, Cookie).

%%--------------------------------------------------------------------
%% Adhoc tests
%%--------------------------------------------------------------------

simple_archive_request(Config) ->
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
        %% Alice sends "OH, HAI!" to Bob
        %% {xmlel,<<"message">>,
        %%  [{<<"from">>,<<"alice@localhost/res1">>},
        %%   {<<"to">>,<<"bob@localhost/res1">>},
        %%   {<<"xml:lang">>,<<"en">>},
        %%   {<<"type">>,<<"chat">>}],
        %%   [{xmlel,<<"body">>,[],[{xmlcdata,<<"OH, HAI!">>}]}]}
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
        escalus:send(Alice, stanza_archive_request(P, <<"q1">>, ?BBOB)),
        assert_respond_size(1, wait_archive_respond_iq_first(Alice)),
        ok
        end,
    MongooseMetrics = [{[backends, mod_mam, archive], changed},
                       {[backends, mod_mam, lookup], changed}
                      ],
    Config1 = [{mongoose_metrics, MongooseMetrics} | Config],
    escalus:story(Config1, [{alice, 1}, {bob, 1}], F).

querying_for_all_messages_with_jid(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        Pregenerated = ?config(pre_generated_msgs, Config),

        WithBob = [true || {_, _, JID, _} <- Pregenerated, JID =:= ?BBOB],

        CountWithBob = length(WithBob),
        escalus:send(Alice, stanza_filtered_by_jid_request(P, ?BBOB)),
        assert_respond_size(CountWithBob, wait_archive_respond_iq_first(Alice)),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

querying_for_all_messages_with_no_jid(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        Pregenerated = ?config(pre_generated_msgs, Config),

        Count = length(Pregenerated),
        escalus:send(Alice, stanza_filtered_by_jid_request(P, <<"">>)),
        assert_respond_size(Count, wait_archive_respond_iq_first(Alice)),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

wait_archive_respond_iq_first(User) ->
    %% rot1
    [IQ|Messages] = lists:reverse(wait_archive_respond(User)),
    ct:log("Msgs: ~p", [[IQ | Messages]]),
    [IQ|lists:reverse(Messages)].

wait_archive_respond(User) ->
    S = escalus:wait_for_stanza(User, 5000),
    case escalus_pred:is_iq_error(S) of
        true ->
            ct:pal("Stanza ~p", [S]),
            ct:fail("Unexpected error IQ.", []);
        false -> ok
    end,
    case escalus_pred:is_iq_result(S) of
        true  -> [S];
        false -> [S|wait_archive_respond(User)]
    end.

assert_respond_size(Size, Respond) when length(Respond) =:= (Size + 1) ->
    Respond;
assert_respond_size(ExpectedSize, Respond) ->
    RespondSize = length(Respond) - 1,
    ct:fail("Respond size is ~p, ~p is expected.", [RespondSize, ExpectedSize]).
    %% void()

%% @doc Querying the archive for all messages in a certain timespan.
range_archive_request(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Send
        %% <iq type='get'>
        %%   <query xmlns='urn:xmpp:mam:tmp'>
        %%     <start>2010-06-07T00:00:00Z</start>
        %%     <end>2010-07-07T13:23:54Z</end>
        %%   </query>
        %% </iq>
        escalus:send(Alice,
                     stanza_date_range_archive_request(P, ?BBOB)),
        IQ = escalus:wait_for_stanza(Alice, 5000),
        escalus:assert(is_iq_result, IQ),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

range_archive_request_not_empty(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        Msgs = msgs_with_user(?BBOB, Config),
        [_, _, StartMsg, StopMsg | _] = Msgs,
        {{StartMsgId, _}, _, _, _StartMsgPacket} = StartMsg,
        {{StopMsgId, _}, _, _, _StopMsgPacket} = StopMsg,
        {StartMicro, _} = rpc_apply(mod_mam_utils, decode_compact_uuid,
                                    [StartMsgId]),
        {StopMicro, _} = rpc_apply(mod_mam_utils, decode_compact_uuid,
                                   [StopMsgId]),
        StartTime = make_iso_time(StartMicro),
        StopTime = make_iso_time(StopMicro),
        %% Send
        %% <iq type='get'>
        %%   <query xmlns='urn:xmpp:mam:tmp'>
        %%     <start>StartTime</start>
        %%     <end>StopTime</end>
        %%   </query>
        %% </iq>
        escalus:send(Alice,
                     stanza_date_range_archive_request_not_empty(P, StartTime,
                                                                 StopTime,
                                                                 ?BBOB)),
        %% Receive two messages and IQ
        M1 = escalus:wait_for_stanza(Alice, 5000),
        ct:log("M1: ~p", [M1]),
        M2 = escalus:wait_for_stanza(Alice, 5000),
        IQ = escalus:wait_for_stanza(Alice, 5000),
        escalus:assert(is_iq_result, IQ),
        #forwarded_message{delay_stamp=Stamp1} = parse_forwarded_message(M1),
        #forwarded_message{delay_stamp=Stamp2} = parse_forwarded_message(M2),
        ?ASSERT_EQUAL(list_to_binary(StartTime), Stamp1),
        ?ASSERT_EQUAL(list_to_binary(StopTime), Stamp2),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

make_iso_time(Micro) ->
    Now = usec_to_now(Micro),
    ct:log("Micro: ~p Now: ~p", [Micro, Now]),
    DateTime = calendar:now_to_datetime(Now),
    {Time, TimeZone} = rpc_apply(jlib, timestamp_to_iso, [DateTime, utc]),
    Time ++ TimeZone.

-define(MILLION, 1000000).
usec_to_now(Usecs) ->
    {Usecs div (?MILLION * ?MILLION),
     Usecs div ?MILLION rem ?MILLION,
     Usecs rem ?MILLION}.

%% @doc A query using Result Set Management.
%% See also `#rsm_in.max'.
limit_archive_request(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Send
        %% <iq type='get' id='q29302'>
        %%   <query xmlns='urn:xmpp:mam:tmp'>
        %%       <start>2010-08-07T00:00:00Z</start>
        %%       <set xmlns='http://jabber.org/protocol/rsm'>
        %%          <limit>10</limit>
        %%       </set>
        %%   </query>
        %% </iq>
        escalus:send(Alice, stanza_limit_archive_request(P, ?BBOB)),
        [IQ | Msgs] = wait_archive_respond_iq_first(Alice),
        escalus:assert(is_iq_result, IQ),
        10 = length(Msgs),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

pagination_empty_rset(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Test the empty result set - should just return the total number of
        %% archived messages
        RSM = #rsm_in{max=0},

        escalus:send(Alice,
            stanza_page_archive_request(P, <<"empty_rset">>, RSM, ?BBOB)),
        wait_empty_rset(P, Alice, 15)
        end,
    escalus:story(Config, [{alice, 1}], F).

pagination_first5(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get the first page of size 5.
        RSM = #rsm_in{max=5},
        escalus:send(Alice,
            stanza_page_archive_request(P, <<"first5">>, RSM, ?BBOB)),
        wait_message_range(P, Alice, 1, 5),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

pagination_first5_opt_count(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get the first page of size 5.
        RSM = #rsm_in{max=5},
        escalus:send(Alice,
            stanza_page_archive_request(P, <<"first5_opt">>, RSM, ?BBOB)),
        wait_message_range(P, Alice, 1, 5),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

pagination_first25_opt_count_all(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get the first page of size 25.
        RSM = #rsm_in{max=25},
        escalus:send(Alice,
            stanza_page_archive_request(P, <<"first25_opt_all">>, RSM, ?BBOB)),
        wait_message_range(P, Alice, 1, 15),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

pagination_last5(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get the last page of size 5.
        RSM = #rsm_in{max=5, direction=before},
        escalus:send(Alice,
            stanza_page_archive_request(P, <<"last5">>, RSM, ?BBOB)),
        wait_message_range(P, Alice, 11, 15),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

pagination_last5_opt_count(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get the last page of size 5.
        RSM = #rsm_in{max=5, direction=before, opt_count=true},
        escalus:send(Alice,
            stanza_page_archive_request(P, <<"last5_opt">>, RSM, ?BBOB)),
        wait_message_range(P, Alice, 11, 15),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

pagination_last25_opt_count_all(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get the last page of size 25.
        RSM = #rsm_in{max=25, direction=before, opt_count=true},
        escalus:send(Alice,
            stanza_page_archive_request(P, <<"last25_opt_all">>, RSM, ?BBOB)),
        wait_message_range(P, Alice, 1, 15),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

pagination_offset5_opt_count(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Skip 5 messages, get 5 messages.
        RSM = #rsm_in{max=5, index=5, opt_count=true},
        escalus:send(Alice,
            stanza_page_archive_request(P, <<"last5_opt">>, RSM, ?BBOB)),
        wait_message_range(P, Alice, 6, 10),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

pagination_offset5_opt_count_all(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Skip 5 messages, get 25 messages (only 10 are available).
        RSM = #rsm_in{max=25, index=5, opt_count=true},
        escalus:send(Alice,
            stanza_page_archive_request(P, <<"last5_opt_all">>, RSM, ?BBOB)),
        wait_message_range(P, Alice, 6, 15),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).


pagination_before10(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get the last page of size 5.
        RSM = #rsm_in{max=5, direction=before, id=message_id(10, Config)},
        escalus:send(Alice,
            stanza_page_archive_request(P, <<"before10">>, RSM, ?BBOB)),
        wait_message_range(P, Alice, 5, 9),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

pagination_simple_before10(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get the last page of size 5.
        RSM = #rsm_in{max=5, direction=before,
                      id=message_id(10, Config), simple=true},
        escalus:send(Alice,
            stanza_page_archive_request(P, <<"before10">>, RSM, ?BBOB)),
     %% wait_message_range(P, Client, TotalCount,    Offset, FromN, ToN),
        wait_message_range(P, Alice,   undefined, undefined,     5,   9),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

pagination_after10(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get the last page of size 5.
        RSM = #rsm_in{max=5, direction='after', id=message_id(10, Config)},
        escalus:send(Alice,
            stanza_page_archive_request(P, <<"after10">>, RSM, ?BBOB)),
        wait_message_range(P, Alice, 11, 15),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

%% Select first page of recent messages after last known id.
%% Paginating from newest messages to oldest ones.
pagination_last_after_id5(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get the last page of size 5 after 5-th message.
        RSM = #rsm_in{max=5, direction='before',
                after_id=message_id(5, Config)},
        escalus:send(Alice,
            stanza_page_archive_request(P, <<"last_after_id5">>, RSM, ?BBOB)),
     %% wait_message_range(P, Client, TotalCount, Offset, FromN, ToN),
        wait_message_range(P, Alice,          10,      5,    11,  15),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

%% Select second page of recent messages after last known id.
pagination_last_after_id5_before_id11(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        RSM = #rsm_in{max=5, direction='before',
                after_id=message_id(5, Config),
                before_id=message_id(11, Config)},
        escalus:send(Alice,
            stanza_page_archive_request(P, <<"last_after_id5_before_id11">>,
                                        RSM, ?BBOB)),
     %% wait_message_range(P, Client, TotalCount, Offset, FromN, ToN),
        wait_message_range(P, Alice,           5,      0,     6,  10),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

generate_message_text(N) when is_integer(N) ->
    <<"Message #", (list_to_binary(integer_to_list(N)))/binary>>.

mam_service_discovery(Config) ->
    F = fun(Alice) ->
        Server = escalus_client:server(Alice),
        escalus:send(Alice, escalus_stanza:disco_info(Server)),
        Stanza = escalus:wait_for_stanza(Alice),
        try
        escalus:assert(is_iq_result, Stanza),
        escalus:assert(has_feature, [mam_ns_binary()], Stanza),
        ok
        catch Class:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            ct:pal("Stanza ~p.", [Stanza]),
            erlang:raise(Class, Reason, Stacktrace)
        end
        end,
    escalus:story(Config, [{alice, 1}], F).

iq_spoofing(Config) ->
    F = fun(Alice, Bob) ->
        %% Sending iqs between clients is allowed.
        %% Every client MUST check "from" and "id" attributes.
        %% This test checks, that server assign corrent "from" attribute
        %% when it is not specified.
        To = escalus_utils:get_jid(Alice),
        From = escalus_utils:get_jid(Bob),
        escalus:send(Bob, escalus_stanza:to(result_iq(), To)),
        Stanza = escalus:wait_for_stanza(Alice),
        escalus_assert:is_stanza_from(From, Stanza),
        escalus_assert:has_no_stanzas(Alice),
        escalus_assert:has_no_stanzas(Bob),
        ok
        end,
    escalus:story(Config, [{alice, 1}, {bob, 1}], F).

reverse(Config) ->
    P = ?config(props, Config),
    F = fun(Alice) ->
        %% Get the first page of size 5.
        RSM = #rsm_in{max=5},
        escalus:send(Alice,
            stanza_reverse_page_request(P, <<"first5">>, RSM)),
        wait_message_range(P, Alice, 15, 0, 5, 1),
        ok
        end,
    escalus:story(Config, [{alice, 1}], F).

result_iq() ->
    #xmlel{
        name = <<"iq">>,
        attrs = [{<<"id">>, <<"xxx">>}, {<<"type">>, <<"result">>}],
        children = [#xmlel{name = <<"query">>}]}.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

mam_ns_binary() -> <<"urn:xmpp:mam:tmp">>.

mam_ns_attr(P) ->
    [{<<"xmlns">>,get_prop(mam_ns, P)}].

skip_undefined(Xs) ->
    [X || X <- Xs, X =/= undefined].

maybe_attr(_, undefined) ->
    [];
maybe_attr(K, V) ->
    [{K, V}].

mam_ns_attr() ->
    [{<<"xmlns">>, mam_ns_binary()}].

maybe_start_elem(undefined) ->
    undefined;
maybe_start_elem(BStart) ->
    #xmlel{
        name = <<"start">>,
        children = #xmlcdata{content = BStart}}.

maybe_end_elem(undefined) ->
    undefined;
maybe_end_elem(BEnd) ->
    #xmlel{
        name = <<"end">>,
        children = #xmlcdata{content = BEnd}}.

maybe_with_elem(undefined) ->
    undefined;
maybe_with_elem(BWithJID) ->
    #xmlel{
        name = <<"with">>,
        children = #xmlcdata{content = BWithJID}}.

%% An optional 'queryid' attribute allows the client to match results to
%% a certain query.
stanza_archive_request(P, QueryId, BWithJID) ->
    stanza_lookup_messages_iq(P, QueryId,
                              undefined, undefined,
                              BWithJID, undefined).

stanza_date_range_archive_request(P, BWithJID) ->
    stanza_lookup_messages_iq(P, undefined,
                              "2010-06-07T00:00:00Z", "2010-07-07T13:23:54Z",
                              BWithJID, undefined).

stanza_date_range_archive_request_not_empty(P, Start, Stop, BWithJID) ->
    stanza_lookup_messages_iq(P, undefined,
                              Start, Stop,
                              BWithJID, undefined).

stanza_limit_archive_request(P, BWithJID) ->
    stanza_lookup_messages_iq(P, undefined, "2010-08-07T00:00:00Z",
                              undefined, BWithJID, #rsm_in{max=10}).

stanza_page_archive_request(P, QueryId, RSM, BWithJID) ->
    stanza_lookup_messages_iq(P, QueryId, undefined, undefined, BWithJID, RSM).

stanza_filtered_by_jid_request(P, BWithJID) ->
    stanza_lookup_messages_iq(P, undefined, undefined,
                              undefined, BWithJID, undefined).

stanza_reverse_page_request(P, QueryId, RSM) ->
    stanza_lookup_messages_iq(P, QueryId, undefined, undefined, undefined,
                              RSM#rsm_in{reverse = true}).

stanza_lookup_messages_iq(P, QueryId, BStart, BEnd, BWithJID, RSM) ->
    case get_prop(data_form, P) of
        false ->
            stanza_lookup_messages_iq_v02(P, QueryId, BStart, BEnd,
                                          BWithJID, RSM);
        true ->
            stanza_lookup_messages_iq_v03(P, QueryId, BStart, BEnd,
                                          BWithJID, RSM)
    end.

get_prop(Key, undefined) ->
    get_prop(Key, []);
get_prop(mam_ns, P) ->
    proplists:get_value(mam_ns, P, mam_ns_binary());
get_prop(result_format, P) ->
    proplists:get_value(result_format, P, iq_query);
get_prop(data_form, P) ->
    proplists:get_bool(data_form, P).

stanza_lookup_messages_iq_v02(_P, QueryId, BStart, BEnd,
                              BWithJID, RSM) ->
    ct:log("Using v02"),
    escalus_stanza:iq(<<"get">>, [#xmlel{
       name = <<"query">>,
       attrs = mam_ns_attr()
            ++ maybe_attr(<<"queryid">>, QueryId)
            ++ border_attributes(RSM),
       children = skip_undefined([
           maybe_simple_elem(RSM),
           maybe_opt_count_elem(RSM),
           maybe_start_elem(BStart),
           maybe_end_elem(BEnd),
           maybe_with_elem(BWithJID),
           maybe_rsm_elem(RSM)])
    }]).

maybe_simple_elem(#rsm_in{simple=true}) ->
    [#xmlel{name = <<"simple">>}];
maybe_simple_elem(_) ->
    [].

maybe_opt_count_elem(#rsm_in{opt_count=true}) ->
    [#xmlel{name = <<"opt_count">>}];
maybe_opt_count_elem(_) ->
    [].

border_attributes(undefined) ->
    [];
border_attributes(#rsm_in{
        before_id=BeforeId, after_id=AfterId, from_id=FromId, to_id=ToId}) ->
    maybe_attr(<<"before_id">>, BeforeId)
    ++ maybe_attr(<<"after_id">>, AfterId)
    ++ maybe_attr(<<"from_id">>, FromId)
    ++ maybe_attr(<<"to_id">>, ToId).

maybe_rsm_elem(undefined) ->
    undefined;
maybe_rsm_elem(#rsm_in{max=Max, direction=Direction, id=Id,
                       index=Index, reverse=Reverse}) ->
    #xmlel{name = <<"set">>,
           children = skip_undefined([
                maybe_rsm_max(Max),
                maybe_rsm_index(Index),
                maybe_rsm_direction(Direction, Id),
                maybe_rsm_reverse(Reverse)])}.

maybe_rsm_id(undefined) -> [];
maybe_rsm_id(Id) -> #xmlcdata{content = Id}.

maybe_rsm_direction(undefined, undefined) ->
    undefined;
maybe_rsm_direction(Direction, Id) ->
    #xmlel{
        name = atom_to_binary(Direction, latin1),
        children = maybe_rsm_id(Id)}.

maybe_rsm_index(undefined) ->
    undefined;
maybe_rsm_index(Index) when is_integer(Index) ->
    #xmlel{
        name = <<"index">>,
        children = #xmlcdata{content = integer_to_list(Index)}}.

maybe_rsm_max(undefined) ->
    undefined;
maybe_rsm_max(Max) when is_integer(Max) ->
    #xmlel{
        name = <<"max">>,
        children = #xmlcdata{content = integer_to_list(Max)}}.

maybe_rsm_reverse(false) ->
    undefined;
maybe_rsm_reverse(true) ->
    #xmlel{name = <<"reverse">>}.

stanza_lookup_messages_iq_v03(P, QueryId, BStart, BEnd,
                              BWithJID, RSM) ->
    ct:log("Using v03"),
    escalus_stanza:iq(<<"set">>, [#xmlel{
       name = <<"query">>,
       attrs = mam_ns_attr(P)
            ++ maybe_attr(<<"queryid">>, QueryId),
       children = skip_undefined([
           form_x(BStart, BEnd, BWithJID, RSM),
           maybe_rsm_elem(RSM)])
    }]).


form_x(BStart, BEnd, BWithJID, RSM) ->
    #xmlel{name = <<"x">>,
           attrs = [{<<"xmlns">>, <<"jabber:x:data">>}],
           children = skip_undefined([
                form_field(<<"start">>, BStart),
                form_field(<<"end">>, BEnd),
                form_field(<<"with">>, BWithJID)]
                ++ form_extra_fields(RSM)
                ++ form_border_fields(RSM))}.

form_reverse_field(false) -> undefined;
form_reverse_field(true) -> form_field(<<"reverse">>, <<"true">>).

form_extra_fields(undefined) ->
    [];
form_extra_fields(#rsm_in{simple=Simple, opt_count=OptCount}) ->
    [form_bool_field(<<"simple">>, Simple),
     form_bool_field(<<"opt_count">>, OptCount)].

form_border_fields(undefined) ->
    [];
form_border_fields(#rsm_in{
        before_id=BeforeId, after_id=AfterId, from_id=FromId, to_id=ToId}) ->
    [form_field(<<"before_id">>, BeforeId),
     form_field(<<"after_id">>, AfterId),
     form_field(<<"from_id">>, FromId),
     form_field(<<"to_id">>, ToId)].

form_field(_VarName, undefined) ->
    undefined;
form_field(VarName, VarValue) ->
    #xmlel{name = <<"field">>, attrs = [{<<"var">>, VarName}],
           children = [#xmlel{name = <<"value">>,
                              children = [#xmlcdata{content = VarValue}]}]}.

form_bool_field(Name, true) ->
    form_field(Name, <<"true">>);
form_bool_field(_Name, _) ->
    undefined.



%% ----------------------------------------------------------------------
%% PARSING RESPONDS

parse_forwarded_message(#xmlel{name = <<"message">>,
                               attrs = Attrs, children = Children}) ->
    M = #forwarded_message{
        from = proplists:get_value(<<"from">>, Attrs),
        to   = proplists:get_value(<<"to">>, Attrs)},
    lists:foldl(fun parse_children_message/2, M, Children).

parse_children_message(#xmlel{name = <<"result">>,
                                 attrs = Attrs,
                                 children = Children}, M) ->
    M1 = M#forwarded_message{
        result_queryid = proplists:get_value(<<"queryid">>, Attrs),
        result_id      = proplists:get_value(<<"id">>, Attrs)},
    lists:foldl(fun parse_children_message_result/2, M1, Children).

parse_children_message_result(#xmlel{name = <<"forwarded">>,
                                        children = Children}, M) ->
    lists:foldl(fun parse_children_message_result_forwarded/2, M, Children).


parse_children_message_result_forwarded(#xmlel{name = <<"delay">>,
                                                  attrs = Attrs}, M) ->
    M#forwarded_message{
        delay_from  = proplists:get_value(<<"from">>, Attrs),
        delay_stamp = proplists:get_value(<<"stamp">>, Attrs)};
parse_children_message_result_forwarded(#xmlel{name = <<"message">>,
                                                  attrs = Attrs,
                                                  children = Children}, M) ->
    M1 = M#forwarded_message{
        message_to   = proplists:get_value(<<"to">>, Attrs),
        message_type = proplists:get_value(<<"type">>, Attrs)},
    lists:foldl(fun parse_children_message_result_forwarded_message/2,
                M1, Children).

parse_children_message_result_forwarded_message(#xmlel{name = <<"body">>,
        children = [{xmlcdata, Body}]}, M) ->
    M#forwarded_message{message_body = Body};
%% Parse `<archived />' here.
parse_children_message_result_forwarded_message(_, M) ->
    M.

%% Num is 1-based.
message_id(Num, Config) ->
    AllMessages = proplists:get_value(all_messages, Config),
    #forwarded_message{result_id=Id} = lists:nth(Num, AllMessages),
    Id.


%% @doc Result query iq.
%%
%% [{xmlel,<<"iq">>,
%%     [{<<"from">>,<<"alice@localhost">>},
%%      {<<"to">>,<<"alice@localhost/res1">>},
%%      {<<"id">>,<<"387862024ce65379b049e19751e4309e">>},
%%      {<<"type">>,<<"result">>}],
%%     []}]
%%
%%
%%  [{xmlel,<<"iq">>,
%%       [{<<"from">>,<<"alice@localhost">>},
%%        {<<"to">>,<<"alice@localhost/res1">>},
%%        {<<"id">>,<<"c256a18c4b720465e215a81362d41eb7">>},
%%        {<<"type">>,<<"result">>}],
%%       [{xmlel,<<"query">>,
%%            [{<<"xmlns">>,<<"urn:xmpp:mam:tmp">>}],
%%            [{xmlel,<<"set">>,
%%                 [{<<"xmlns">>,<<"http://jabber.org/protocol/rsm">>}],
%%                 [{xmlel,<<"first">>,
%%                      [{<<"index">>,<<"10">>}],
%%                      [{xmlcdata,<<"103439">>}]},
%%                  {xmlel,<<"last">>,[],[{xmlcdata,<<"103447">>}]},
%%                  {xmlel,<<"count">>,[],[{xmlcdata,<<"15">>}]}]}]}]}]

parse_result_iq(P, Result) ->
    case get_prop(result_format, P) of
        iq_query ->
            parse_legacy_iq(Result);
        iq_fin ->
            parse_fin_iq(Result)
    end.

%% MAM v0.4
parse_fin_iq(IQ) ->
    Fin = exml_query:subelement(IQ, <<"fin">>),
    Set = exml_query:subelement(Fin, <<"set">>),
    parse_set_and_iq(IQ, Set).

%% MAM v0.2
parse_legacy_iq(IQ) ->
    Fin = exml_query:subelement(IQ, <<"query">>),
    Set = exml_query:subelement(Fin, <<"set">>),
    parse_set_and_iq(IQ, Set).

parse_set_and_iq(IQ, Set) ->
    #result_iq{
        from        = exml_query:attr(IQ, <<"from">>),
        to          = exml_query:attr(IQ, <<"to">>),
        id          = exml_query:attr(IQ, <<"id">>),
        first       = exml_query:path(Set, [{element, <<"first">>}, cdata]),
        first_index = maybe_binary_to_integer(
                        exml_query:path(Set, [{element, <<"first">>},
                                              {attr, <<"index">>}])),
        last        = exml_query:path(Set, [{element, <<"last">>}, cdata]),
        count       = maybe_binary_to_integer(
                        exml_query:path(Set, [{element, <<"count">>}, cdata]))}.

maybe_binary_to_integer(B) when is_binary(B) ->
    list_to_integer(binary_to_list(B));
maybe_binary_to_integer(undefined) ->
    undefined.

send_rsm_messages(Config) ->
    Pid = self(),
    P = ?config(props, Config),
    F = fun(Alice, Bob) ->
        %% Alice sends messages to Bob.
        [escalus:send(Alice,
                      escalus_stanza:chat_to(Bob, generate_message_text(N)))
         || N <- lists:seq(1, 15)],
        %% Bob is waiting for 15 messages for 5 seconds.
        R = escalus:wait_for_stanzas(Bob, 15, 5000),
        15 = length(R),
        %% Get whole history.
        escalus:send(Alice,
                     stanza_archive_request(P, <<"all_messages">>, ?BBOB)),
        [_ArcIQ|AllMessages] =
            assert_respond_size(15, wait_archive_respond_iq_first(Alice)),
        ParsedMessages = [parse_forwarded_message(M) || M <- AllMessages],
        Pid ! {parsed_messages, ParsedMessages},
        ok
        end,
    Config1 = escalus:init_per_testcase(pre_rsm, Config),
    escalus:story(Config1, [{alice, 1}, {bob, 1}], F),
    ParsedMessages = receive {parsed_messages, PM} -> PM
                     after 5000 -> error(receive_timeout) end,

    escalus:end_per_testcase(pre_rsm, Config1),
    [{all_messages, ParsedMessages}|Config].

clean_archives(Config) ->
    SUs = serv_users(Config),
    %% It is not the best place to delete these messages.
    [ok = delete_offline_messages(S, U) || {S, U} <- SUs],
    [ok = delete_archive(S, U) || {S, U} <- SUs],
    timer:sleep(1500),
    [assert_empty_archive(S, U) || {S, U} <- SUs],
    Config.

serv_users(Config) ->
    [serv_user(Config, UserSpec)
     || {_, UserSpec} <- escalus_users:get_users(all)].

serv_user(Config, UserSpec) ->
    [Username, Server, _Pass] = escalus_users:get_usp(Config, UserSpec),
    {Server, Username}.

%% @doc Check, that the archive is empty.
assert_empty_archive(Server, Username) ->
    case archive_size(Server, Username) of
       0 -> ok;
       X -> ct:fail({not_empty, Server, Username, X})
    end.

archive_size(Server, Username) ->
    rpc_apply(mod_mam, archive_size, [Server, Username]).

delete_archive(Server, Username) ->
    rpc_apply(mod_mam, delete_archive, [Server, Username]).

delete_offline_messages(Server, Username) ->
    %% Do not care
    catch rpc_apply(mod_offline, remove_user, [Username, Server]),
    ok.

wait_message_range(P, Client, FromN, ToN) ->
    wait_message_range(P, Client, 15, FromN-1, FromN, ToN).

wait_message_range(P, Client, TotalCount, Offset, FromN, ToN) ->
    [IQ|Messages] = wait_archive_respond_iq_first(Client),
    ParsedMessages = parse_messages(Messages),
    ParsedIQ = parse_result_iq(P, IQ),
    try
        %% Compare body of the messages.
        ?ASSERT_EQUAL([generate_message_text(N) || N <- make_seq(FromN, ToN)],
                      [B || #forwarded_message{message_body=B}
                            <- ParsedMessages]),
        ?ASSERT_EQUAL(TotalCount, ParsedIQ#result_iq.count),
        ?ASSERT_EQUAL(Offset, ParsedIQ#result_iq.first_index),
        ok
    catch Class:Reason ->
        Stacktrace = erlang:get_stacktrace(),
        ct:pal("IQ: ~p~n"
               "Messages: ~p~n"
               "Parsed messages: ~p~n",
               [IQ, Messages, ParsedMessages]),
        erlang:raise(Class, Reason, Stacktrace)
    end.

make_seq(FromN, ToN) when ToN < FromN ->
    lists:reverse(make_seq(ToN, FromN));
make_seq(FromN, ToN) ->
    lists:seq(FromN, ToN).

wait_empty_rset(P, Alice, TotalCount) ->
    [IQ] = wait_archive_respond_iq_first(Alice),
    ParsedIQ = parse_result_iq(P, IQ),
    try
        ?ASSERT_EQUAL(TotalCount, ParsedIQ#result_iq.count),
        ok
    catch Class:Reason ->
        Stacktrace = erlang:get_stacktrace(),
        ct:pal("IQ: ~p~n", [IQ]),
        erlang:raise(Class, Reason, Stacktrace)
    end.

parse_messages(Messages) ->
    try [parse_forwarded_message(M) || M <- Messages]
    catch Class:Reason ->
        Stacktrace = erlang:get_stacktrace(),
        ct:pal("Messages: ~p~n", [Messages]),
        erlang:raise(Class, Reason, Stacktrace)
    end.

bootstrap_archive(Config) ->
    ArcJID = ?BALICE,
    OtherUsers = [?BBOB, ?BCAROL],
    Msgs = generate_msgs_for_days(ArcJID, OtherUsers, 16),
    ets:new(used_times, [private, named_table]),
    put_msgs(Msgs),
    ets:delete(used_times),
    timer:sleep(1500),
    [{pre_generated_msgs, sort_msgs(Msgs)} | Config].

sort_msgs(Msgs) ->
    SortFun = fun({{ID1, _}, _, _, _}, {{ID2, _}, _, _, _}) ->
        ID1 =< ID2
    end,
    lists:sort(SortFun, Msgs).

generate_msgs_for_days(OwnerJID, OtherUsers, Days) ->
    {TodayDate, _} = calendar:local_time(),
    Today = calendar:date_to_gregorian_days(TodayDate),
    StartDay = Today - Days,
    lists:flatten([generate_msgs_for_day(Day, OwnerJID, OtherUsers)
                   || Day <- lists:seq(StartDay, Today)]).

generate_msgs_for_day(Day, OwnerJID, OtherUsers) ->
    Date = calendar:gregorian_days_to_date(Day),

    [generate_msg_for_date_user(OwnerJID, RemoteJID, {Date, random_time()})
     || RemoteJID <- OtherUsers].

generate_msg_for_date_user(OwnerJID, RemoteJID, DateTime) ->
    MicrosecDateTime = datetime_to_microseconds(DateTime),
    NowMicro = rpc_apply(mod_mam_utils, now_to_microseconds,
                         [rpc_apply(erlang, now, [])]),
    Microsec = min(NowMicro, MicrosecDateTime),
    MsgIdOwner = rpc_apply(mod_mam_utils, encode_compact_uuid,
                           [Microsec, rand:uniform(20)]),
    MsgIdRemote = rpc_apply(mod_mam_utils, encode_compact_uuid,
                            [Microsec+1, rand:uniform(20)]),
    Packet = escalus_stanza:chat_to(RemoteJID,
                                    base16:encode(crypto:strong_rand_bytes(4))),
    {{MsgIdOwner, MsgIdRemote}, OwnerJID, RemoteJID, Packet}.

random_time() ->
    MaxSecondsInDay = 86399,
    RandSeconds = rand:uniform(MaxSecondsInDay),
    calendar:seconds_to_time(RandSeconds).

datetime_to_microseconds({{_, _, _}, {_, _, _}} = DateTime) ->
    S1 = calendar:datetime_to_gregorian_seconds(DateTime),
    S0 = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Seconds = S1 - S0,
    Seconds * 1000000.


put_msgs(Msgs) ->
    [put_msg(Msg) || Msg <- Msgs].

put_msg({{MsgIdOwner, MsgIdRemote},
         OwnerBJID,
         RemoteBJID,
         Packet}) ->
    Host = escalus_ct:get_config(ejabberd_domain),
    OwnerJID = jid:from_binary(OwnerBJID),
    RemoteJID = jid:from_binary(RemoteBJID),
    {FromTS, _} = rpc_apply(mod_mam_utils, decode_compact_uuid,
                                [MsgIdOwner]),
    archive_message([Host, MsgIdOwner, OwnerJID, RemoteJID,
                     outgoing, Packet, ensure_unique(FromTS div 1000)]),
    {ToTS, _} = rpc_apply(mod_mam_utils, decode_compact_uuid,
                                [MsgIdRemote]),
    archive_message([Host, MsgIdRemote, RemoteJID, OwnerJID,
                     incoming, Packet, ensure_unique(ToTS div 1000)]).

archive_message(Args) ->
    rpc_apply(mod_wocky_mam, archive_test_message, Args).


%% @doc Get a binary jid of the user, that tagged with `Username' in the config.
nick_to_bjid(Username, Config) when is_atom(Username) ->
    UserSpec = escalus_users:get_userspec(Config, Username),
    escalus_utils:jid_to_lower(escalus_users:get_jid(Config, UserSpec)).

msgs_with_user(BJID, Config) ->
    Msgs = ?config(pre_generated_msgs, Config),
    lists:filter(fun(M) -> msg_has_user(BJID, M) end, Msgs).

msg_has_user(User, {_, A, B, _}) -> A =:= User orelse B =:= User.

ensure_unique(T) ->
    case ets:lookup(used_times, T) of
        [] ->
            ets:insert(used_times, {T, true}),
            T;
        _ ->
            ensure_unique(T+1)
    end.
