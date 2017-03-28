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
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml_stream.hrl").

-include("wocky_db_seed.hrl").

-compile({parse_transform, fun_chain}).

-define(ASSERT_EQUAL(E, V), (
    [ct:fail("ASSERT EQUAL~n\tExpected ~p~n\tValue ~p~n", [(E), (V)])
     || (E) =/= (V)]
    )).

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
     {group, rsm},
     {group, rsm_04}
    ].

groups() ->
    [
     {rsm, [], rsm_cases()},
     {rsm_04, [], rsm_cases()}
    ].

rsm_cases() ->
    [
     %% Reversed result list
     reverse
    ].

suite() ->
    escalus:suite().

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    wocky_db:clear_user_tables(?LOCAL_CONTEXT),
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
        create_users(),
        test_helper:make_everyone_friends(escalus:get_users(user_names()))
    ),
    init_state(Group, ConfigOut).

end_per_group(_Group, Config) ->
    delete_users(Config).

init_state(rsm, Config) ->
    send_rsm_messages(clean_archives(Config));
init_state(rsm_04, Config) ->
    Config1 = [{props, mam04_props()} | Config],
    send_rsm_messages(clean_archives(Config1)).

mam04_props() ->
    [{data_form, true},                 %% send data forms
     {result_format, iq_fin},           %% RSM is inside iq with <fin/> inside
     {mam_ns, mam_ns_binary_v04()}].

mam_ns_binary_v04() -> <<"urn:xmpp:mam:1">>.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

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
        children = [#xmlcdata{content = BStart}]}.

maybe_end_elem(undefined) ->
    undefined;
maybe_end_elem(BEnd) ->
    #xmlel{
        name = <<"end">>,
        children = [#xmlcdata{content = BEnd}]}.

maybe_with_elem(undefined) ->
    undefined;
maybe_with_elem(BWithJID) ->
    #xmlel{
        name = <<"with">>,
        children = [#xmlcdata{content = BWithJID}]}.

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

generate_message_text(N) when is_integer(N) ->
    <<"Message #", (list_to_binary(integer_to_list(N)))/binary>>.

make_seq(FromN, ToN) when ToN < FromN ->
    lists:reverse(make_seq(ToN, FromN));
make_seq(FromN, ToN) ->
    lists:seq(FromN, ToN).

stanza_archive_request(P, QueryId, BWithJID) ->
    stanza_lookup_messages_iq(P, QueryId,
                              undefined, undefined,
                              BWithJID, undefined).

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
    #xmlel{name = <<"simple">>};
maybe_simple_elem(_) ->
    undefined.

maybe_opt_count_elem(#rsm_in{opt_count=true}) ->
    #xmlel{name = <<"opt_count">>};
maybe_opt_count_elem(_) ->
    undefined.

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

maybe_rsm_id(undefined) -> undefined;
maybe_rsm_id(Id) -> #xmlcdata{content = Id}.

maybe_rsm_direction(undefined, undefined) ->
    undefined;
maybe_rsm_direction(Direction, Id) ->
    #xmlel{
        name = atom_to_binary(Direction, latin1),
        children = skip_undefined([maybe_rsm_id(Id)])}.

maybe_rsm_index(undefined) ->
    undefined;
maybe_rsm_index(Index) when is_integer(Index) ->
    #xmlel{
        name = <<"index">>,
        children = [#xmlcdata{content = integer_to_list(Index)}]}.

maybe_rsm_max(undefined) ->
    undefined;
maybe_rsm_max(Max) when is_integer(Max) ->
    #xmlel{
        name = <<"max">>,
        children = [#xmlcdata{content = integer_to_list(Max)}]}.

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

        %% Give the index time to update
        timer:sleep(1000),

        %% Get whole history.
        escalus:send(Alice,
                     stanza_archive_request(P, <<"all_messages">>, ?BOB_B_JID)),

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

assert_respond_size(Size, Respond) when length(Respond) =:= (Size + 1) ->
    Respond;
assert_respond_size(ExpectedSize, Respond) ->
    RespondSize = length(Respond) - 1,
    ct:fail("Respond size is ~p, ~p is expected.", [RespondSize, ExpectedSize]).

parse_messages(Messages) ->
    try [parse_forwarded_message(M) || M <- Messages]
    catch Class:Reason ->
        Stacktrace = erlang:get_stacktrace(),
        ct:pal("Messages: ~p~n", [Messages]),
        erlang:raise(Class, Reason, Stacktrace)
    end.

%% ----------------------------------------------------------------------
%% Archive cleaning

clean_archives(Config) ->
    SUs = serv_users(Config),
    [ok = delete_archive(S, U) || {S, U} <- SUs],
    %% Retry 10 times if not empty
    [assert_empty_archive(S, U, 10) || {S, U} <- SUs],
    Config.

delete_archive(Server, Username) ->
    rpc_apply(mod_mam, delete_archive, [Server, Username]).

archive_size(Server, Username) ->
    rpc_apply(mod_mam, archive_size, [Server, Username]).

serv_users(Config) ->
    [serv_user(Config, UserSpec)
     || {_, UserSpec} <- escalus_users:get_users(all)].

serv_user(Config, UserSpec) ->
    [Username, Server, _Pass] = escalus_users:get_usp(Config, UserSpec),
    {Server, Username}.

%% @doc Check, that the archive is empty.
assert_empty_archive(Server, Username, RetryTimes) when is_integer(RetryTimes) ->
    %% Wait for zero messages in archive
    case wait_for_archive_size(Server, Username, RetryTimes, 0) of
       0 -> ok;
       X -> ct:fail({not_empty, Server, Username, {actual_size, X}})
    end.

wait_for_archive_size(Server, Username, _RetryTimes=0, _ExpectedSize) ->
    archive_size(Server, Username);
wait_for_archive_size(Server, Username, RetryTimes, ExpectedSize) when RetryTimes > 0 ->
    case archive_size(Server, Username) of
        ExpectedSize ->
            ExpectedSize;
        _ActualSize ->
            %% Wait and retry
            timer:sleep(100),
            wait_for_archive_size(Server, Username, RetryTimes-1, ExpectedSize)
    end.
