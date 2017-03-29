%%% @copyright 2016+ Hippware, Inc.
%%% @doc Helper functions for Wocky integration test suites
-module(test_helper).

-include("wocky.hrl").
-include("wocky_db_seed.hrl").
-include("test_helper.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile({parse_transform, cut}).
-compile({parse_transform, do}).

-export([ensure_wocky_is_running/0,
         make_everyone_friends/2
        ]).

-export([expect_iq_success/2,
         expect_iq_error/2,
         expect_iq_success_u/2,
         expect_iq_error_u/2,
         expect_iq_success_u/3,
         expect_iq_error_u/3,

         ensure_all_clean/1,

         expect_friendship_presence/2,
         subscribe/2,
         expect_subscription_stanzas/2,
         add_sample_contact/2,
         subscribe_pair/2,
         add_contact/4,
         add_to_s/2,
         add_to_u/2,
         remove_friend/2,

         iq_set/2,
         iq_get/2,
         iq_with_type/3,

         rsm_elem/1,
         decode_rsm/1,
         check_rsm/5,

         check_attr/3,

         get_hs_stanza/0,
         get_hs_stanza/1,
         check_hs_result/2,
         check_hs_result/4,
         check_single_hs_result/2,
         get_items_el/1,

         publish_item_stanza/4,
         publish_item_stanza/5,
         retract_item_stanza/2,
         subscribe_stanza/0,
         is_bot_action/2,
         is_bot_action/3,

         hs_node/1,
         bot_node/1,
         node_el/2,
         node_el/3,
         cdata_el/2,
         hs_query_el/1,

         set_notifications/2
        ]).

ensure_wocky_is_running() ->
    case net_kernel:start(['mongooseim@localhost', longnames]) of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end,

    ok = wocky_app:start("ct.test").

ensure_all_clean(Clients) ->
    lists:foreach(fun(Client) ->
        escalus_assert:has_no_stanzas(Client)
    end, Clients).

expect_iq_success_u(Stanza, Client) ->
    expect_iq_success_u(Stanza, Client, Client).

expect_iq_error_u(Stanza, Client) ->
    expect_iq_error_u(Stanza, Client, Client).

expect_iq_success_u(Stanza, FromClient, ToClient) ->
    expect_something(add_to_u(Stanza, ToClient), FromClient, is_iq_result).

expect_iq_error_u(Stanza, FromClient, ToClient) ->
    expect_something(add_to_u(Stanza, ToClient), FromClient, is_iq_error).

add_to_u(Stanza, Client) ->
    escalus_stanza:to(Stanza,
      escalus_client:short_jid(Client)).

expect_iq_success(Stanza, Client) ->
    expect_something(add_to_s(Stanza, Client), Client, is_iq_result).

expect_iq_error(Stanza, Client) ->
    expect_something(add_to_s(Stanza, Client), Client, is_iq_error).

expect_something(Stanza, Client, Expect) ->
    ct:log("Sending stanza: ~p", [Stanza]),
    escalus:send(Client, Stanza),
    ResultStanza = escalus:wait_for_stanza(Client, timeout()),
    ct:log("Result stanza: ~p", [ResultStanza]),
    escalus:assert(Expect, ResultStanza),
    ResultStanza.

add_to_s(Stanza, Client) ->
    escalus_stanza:to(Stanza,
      escalus_client:server(Client)).


expect_friendship_presence(User1, User2) ->
    lists:foreach(fun(U) ->
                          [S1, S2] = escalus:wait_for_stanzas(U, 2),
                          escalus:assert(is_presence_stanza, S1),
                          escalus:assert(is_presence_stanza, S2)
                  end,
                  [User1, User2]).

add_contact(Who, Whom, Group, Nick) when is_binary(Group) ->
    add_contact(Who, Whom, [Group], Nick);
add_contact(Who, Whom, Groups, Nick) when is_list(Groups) ->
    escalus_client:send(Who,
                        escalus_stanza:roster_add_contact(Whom,
                                                          Groups,
                                                          Nick)),
    Received = escalus_client:wait_for_stanza(Who),
    escalus_assert:is_roster_set(Received),
    reply_to_roster_set(Who, [Received]),
    escalus:assert(is_iq_result, escalus:wait_for_stanza(Who)).

subscribe(Who, Whom) ->
    %% 'Who' sends a subscribe request to 'Whom'
    escalus:send(Who, escalus_stanza:presence_direct(
                        escalus_client:short_jid(Whom), <<"subscribe">>)),
    PushReq = escalus:wait_for_stanza(Who),
    escalus:assert(is_roster_set, PushReq),
    reply_to_roster_set(Who, [PushReq]),

    %% 'Whom' receives subscription reqest
    %% In wocky contact is auto-accepted and added to roster
    Stanzas = expect_subscription_stanzas(Whom, <<"subscribe">>),
    reply_to_roster_set(Whom, Stanzas),

    %% 'Who' receives subscribed
    expect_subscription_stanzas(Who, <<"subscribed">>).

expect_subscription_stanzas(Who, Type) ->
    Stanzas = escalus:wait_for_stanzas(Who, 2),
    IsPresWithType = fun (S) ->
                         escalus_pred:is_presence_with_type(Type, S)
                     end,
    escalus:assert_many([is_roster_set, IsPresWithType], Stanzas),
    Stanzas.

add_sample_contact(Alice, Bob) ->
    escalus:send(Alice,
                 escalus_stanza:roster_add_contact(Bob, [<<"friends">>],
                                                   <<"Bobby">>)),

    Received = escalus:wait_for_stanzas(Alice, 2),
    escalus:assert_many([is_roster_set, is_iq_result], Received),

    Result = hd([R || R <- Received, escalus_pred:is_roster_set(R)]),
    escalus:assert(count_roster_items, [1], Result),
    escalus:send(Alice, escalus_stanza:iq_result(Result)).

reply_to_roster_set(Client, Stanzas) when is_list(Stanzas) ->
    RosterSet = hd([R || R <- Stanzas, escalus_pred:is_roster_set(R)]),
    reply_to_roster_set(Client, RosterSet);

reply_to_roster_set(Client, RosterSet) ->
    escalus:send(Client, escalus_stanza:iq_result(RosterSet)),
    escalus:assert(count_roster_items, [1], RosterSet).

make_everyone_friends(Config0, Users) ->
    % start the clients
    Config1 = escalus_cleaner:start(Config0),
    Clients = start_clients_before_all_friends(
                Config1, [[{US, <<"friendly">>}] || {_Name, US} <- Users]),

    % exchange subscribe and subscribed stanzas
    escalus_utils:distinct_pairs(fun subscribe_pair/2, Clients),

    ensure_all_clean(Clients),

    % stop the clients
    escalus_cleaner:clean(Config1),
    escalus_cleaner:stop(Config1),

    % return Config0
    [{everyone_is_friends, true} | Config0].

subscribe_pair(Alice, Bob) ->
    subscribe(Alice, Bob),
    subscribe(Bob, Alice),
    Presence = escalus:wait_for_stanza(Bob),
    escalus:assert(is_presence, Presence).

remove_friend(Who, Whom) ->
    escalus:send(Who, escalus_stanza:roster_remove_contact(Whom)),
    escalus:assert_many(
      [is_roster_set, is_iq_result,
       escalus_pred:is_presence_with_type(<<"unavailable">>, _)],
      escalus:wait_for_stanzas(Who, 3)),
    escalus:assert_many(
      [is_roster_set,
       escalus_pred:is_presence_with_type(<<"unsubscribe">>, _),
       escalus_pred:is_presence_with_type(<<"unavailable">>, _)
      ],
      escalus:wait_for_stanzas(Whom, 3)).

start_clients_before_all_friends(Config, ClientDescs) ->
    ct:log("start_clients_all_friends ~p", [ClientDescs]),
    lists:flatmap(fun(UserCDs) ->
                          call_start_ready_clients(Config, UserCDs)
                  end, ClientDescs).

call_start_ready_clients(Config, UserCDs) ->
    escalus_overridables:do(Config, start_ready_clients, [Config, UserCDs],
                            {escalus_story, start_ready_clients}).

iq_get(NS, Payload) ->
    iq_with_type(<<"get">>, NS, Payload).

iq_set(NS, Payload) ->
    iq_with_type(<<"set">>, NS, Payload).

iq_with_type(Type, NS, Payload = #xmlel{attrs = Attrs}) ->
    #xmlel{name = <<"iq">>,
           attrs = [{<<"type">>, Type},
                    {<<"id">>, wocky_util:iq_id()}],
           children = [Payload#xmlel{attrs = [{<<"xmlns">>, NS} | Attrs]}]}.

%% RSM encoding
rsm_elem(#rsm_in{max=Max, direction=Direction, id=Id, index=Index}) ->
    #xmlel{name = <<"set">>,
           children = skip_undefined([
                maybe_rsm_max(Max),
                maybe_rsm_index(Index),
                maybe_rsm_direction(Direction, Id)])}.

rsm_id_children(undefined) -> [];
rsm_id_children(Id) -> [#xmlcdata{content = Id}].

maybe_rsm_direction(undefined, undefined) ->
    undefined;
maybe_rsm_direction(Direction, Id) ->
    #xmlel{
        name = atom_to_dir(Direction),
        children = rsm_id_children(Id)}.

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

skip_undefined(Xs) ->
    [X || X <- Xs, X =/= undefined].

atom_to_dir(aft) -> <<"after">>;
atom_to_dir(before) -> <<"before">>.

%% RSM decoding
decode_rsm(XML) -> decode_rsm(XML, #rsm_out{}).
decode_rsm([], Acc) -> {ok, Acc};
decode_rsm([#xmlel{name = <<"first">>,
                  attrs = [{<<"index">>, Index}],
                  children = [#xmlcdata{content = First}]} | Rest], Acc) ->
    decode_rsm(Rest, Acc#rsm_out{index = binary_to_integer(Index),
                                 first = First});
decode_rsm([#xmlel{name = <<"last">>,
                  children = [#xmlcdata{content = Last}]} | Rest], Acc) ->
    decode_rsm(Rest, Acc#rsm_out{last = Last});
decode_rsm([#xmlel{name = <<"count">>,
                  children = [#xmlcdata{content = Count}]} | Rest], Acc) ->
    decode_rsm(Rest, Acc#rsm_out{count = binary_to_integer(Count)});
decode_rsm([El | _], _) ->
    {error, {unknown_rsm_el, El}}.

check_rsm(#rsm_out{count = Count, index = Index, first = First, last = Last},
          ExpectedCount, ExpectedIndex, ExpectedFirst, ExpectedLast) ->
    case {Count, Index, First, Last} of
        {ExpectedCount, ExpectedIndex, ExpectedFirst, ExpectedLast} ->
            ok;
        E ->
            {error,
             {bad_rsm, E,
              {ExpectedCount, ExpectedIndex, ExpectedFirst, ExpectedLast}}}
    end.

check_attr(Name, Value, #xmlel{attrs = Attrs}) ->
    case wocky_xml:get_attr(Name, Attrs) of
        {ok, Value} -> ok;
        {ok, _} when Value =:= any -> ok;
        X -> {error, {invalid_attr, Name, Value, X}}
    end.

timeout() ->
    case ejabberd_config:get_local_option(tros_backend) of
        s3 -> 10000; % Extra time for S3 latency
        _ -> 1000 % Default in escalus
    end.

check_hs_result(Stanza, NumItems) ->
    check_hs_result(Stanza, NumItems, 0, true).
check_hs_result(Stanza, NumItems, NumDeletes, CheckVersion) ->
    {ok, L} = do([error_m ||
                  ItemsEl <- get_items_el(Stanza),
                  check_attr(<<"node">>, ?HOME_STREAM_NODE, ItemsEl),
                  check_attr(<<"xmlns">>, ?NS_PUBLISHING, ItemsEl),
                  maybe(CheckVersion, check_attr(<<"version">>, any, ItemsEl)),
                  ItemList <- get_items(ItemsEl),
                  check_elements(ItemsEl, NumItems, NumDeletes, 1),
                  {ok, ItemList}
                 ]),
    L.

maybe(false, _) -> ok;
maybe(true, X) -> X.

get_items_el(Stanza) ->
    case xml:get_subtag(Stanza, <<"items">>) of
        false -> {error, no_items};
        El -> {ok, El}
    end.

check_single_hs_result(Stanza, ID) ->
    {ok, L} = do([error_m ||
                  ItemsEl <- test_helper:get_items_el(Stanza),
                  check_attr(<<"node">>, ?HOME_STREAM_NODE, ItemsEl),
                  check_attr(<<"xmlns">>, ?NS_PUBLISHING, ItemsEl),
                  ItemList <- get_items(ItemsEl),
                  check_elements(ItemsEl, 1, 0, 0),
                  {ok, ?assertEqual(ID, (hd(ItemList))#item.id)},
                  {ok, hd(ItemList)}
                 ]),
    L.

get_items(Stanza) ->
    {ok, lists:reverse(lists:foldl(get_item(_, _), [], Stanza#xmlel.children))}.

get_item(#xmlel{name = <<"item">>, attrs = Attrs, children = Children}, Acc) ->
    {value, ID} = xml:get_attr(<<"id">>, Attrs),
    {value, Version} = xml:get_attr(<<"version">>, Attrs),
    {value, From} = xml:get_attr(<<"from">>, Attrs),
    [#item{id = ID,
           version = Version,
           from = From,
           stanzas = Children}
     | Acc];
get_item(#xmlel{name = <<"delete">>, attrs = Attrs, children = []}, Acc) ->
    {value, ID} = xml:get_attr(<<"id">>, Attrs),
    [{delete, ID} | Acc];
get_item(_, Acc) ->
    Acc.

check_elements(Items, NumItems, NumDeletes, NumRSM) ->
    ?assert(NumItems =:= any orelse
            NumItems =:= count_elements(Items, <<"item">>)),
    ?assert(NumDeletes =:= any orelse
            NumDeletes =:= count_elements(Items, <<"delete">>)),
    ?assertEqual(NumRSM, count_elements(Items, <<"set">>)),
    ok.

count_elements(#xmlel{name = <<"items">>, children = Children}, Type) ->
    length(
      lists:filter(fun(#xmlel{name = T}) when T =:= Type -> true;
                      (_) -> false
                   end,
                   Children)).

publish_item_stanza(BotID, NoteID, Title, Content) ->
    publish_item_stanza(BotID, NoteID, Title, Content, undefined).
publish_item_stanza(BotID, NoteID, Title, Content, Image) ->
    iq_set(?NS_BOT, publish_el(BotID, NoteID, Title, Content, Image)).

publish_el(BotID, NoteID, Title, Content, Image) ->
    #xmlel{name = <<"publish">>,
           attrs = [{<<"node">>, bot_node(BotID)}],
           children = [item_el(NoteID, Title, Content, Image)]}.

item_el(NoteID) ->
    #xmlel{name = <<"item">>,
           attrs = [{<<"id">>, NoteID}]}.
item_el(NoteID, Title, Content, Image) ->
    #xmlel{name = <<"item">>,
           attrs = [{<<"id">>, NoteID}],
           children = [entry_el(Title, Content, Image)]}.

entry_el(Title, Content, Image) ->
    #xmlel{name = <<"entry">>,
           attrs = [{<<"xmlns">>, ?NS_ATOM}],
           children = item_fields(Title, Content, Image)}.

item_fields(Title, Content, Image) ->
    [#xmlel{name = <<"title">>,
            children = [#xmlcdata{content = Title}]},
     #xmlel{name = <<"content">>,
            children = [#xmlcdata{content = Content}]}] ++
    maybe_image_el(Image).

maybe_image_el(undefined) -> [];
maybe_image_el(Image) ->
    [#xmlel{name = <<"image">>,
            children = [#xmlcdata{content = Image}]}].

retract_item_stanza(BotID, NoteID) ->
    iq_set(?NS_BOT, retract_el(BotID, NoteID)).

retract_el(BotID, NoteID) ->
    #xmlel{name = <<"retract">>,
           attrs = [{<<"node">>, bot_node(BotID)}],
           children = [item_el(NoteID)]}.

bot_node(ID) ->
    <<"bot/", ID/binary>>.

subscribe_stanza() ->
    SubEl = node_el(?BOT, <<"subscribe">>),
    iq_set(?NS_BOT, SubEl).

node_el(ID, Name) -> node_el(ID, Name, []).
node_el(ID, Name, Children) ->
    #xmlel{name = Name, attrs = [{<<"node">>, bot_node(ID)}],
           children = Children}.

cdata_el(Name, Value) ->
    #xmlel{name = Name, children = [#xmlcdata{content = Value}]}.

is_bot_action(ID, Stanza) ->
    is_bot_action(ID, any, Stanza).
is_bot_action(ID, Action, Stanza) ->
    Stanza#xmlel.name =:= <<"message">> andalso
    xml:get_tag_attr(<<"type">>, Stanza) =:= {value, <<"headline">>} andalso
    xml:get_path_s(Stanza, [{elem, <<"bot">>}, {attr, <<"xmlns">>}])
        =:= ?NS_BOT andalso
    matches(
      xml:get_path_s(Stanza, [{elem, <<"bot">>}, {elem, <<"action">>}, cdata]),
      Action) andalso
    xml:get_path_s(Stanza, [{elem, <<"bot">>}, {elem, <<"id">>}, cdata])
        =:= ID andalso
    xml:get_path_s(Stanza, [{elem, <<"bot">>}, {elem, <<"jid">>}, cdata])
        =:= jid:to_binary(jid:make(<<>>, ?LOCAL_CONTEXT, <<"bot/", ID/binary>>))
        andalso
    xml:get_path_s(Stanza, [{elem, <<"bot">>}, {elem, <<"server">>}, cdata])
        =:= ?LOCAL_CONTEXT.

matches(Value, any) -> Value =/= <<>>;
matches(Value, Match) -> Value =:= Match.


hs_query_el(Version) ->
    #xmlel{name = <<"query">>,
           attrs = [{<<"xmlns">>, ?NS_PUBLISHING} |
                    maybe_version_attr(Version)]}.

maybe_version_attr(undefined) -> [];
maybe_version_attr(Version) -> [{<<"version">>, Version}].

hs_node(User) ->
    jid:to_binary(
      jid:make(User, ?LOCAL_CONTEXT, <<"home_stream">>)).

get_hs_stanza() ->
    get_hs_stanza(#rsm_in{max = 500}).

get_hs_stanza(RSM = #rsm_in{}) ->
    test_helper:iq_get(?NS_PUBLISHING,
                       #xmlel{name = <<"items">>,
                              attrs = [{<<"node">>, ?HOME_STREAM_NODE}],
                              children = [rsm_elem(RSM)]});

get_hs_stanza(ID) when is_binary(ID) ->
    test_helper:iq_get(?NS_PUBLISHING,
                       #xmlel{name = <<"items">>,
                              attrs = [{<<"node">>, ?HOME_STREAM_NODE}],
                              children = [#xmlel{name = <<"item">>,
                                                 attrs = [{<<"id">>, ID}]}]}).

set_notifications(Enabled, Client) ->
    Stanza = notifications_stanza(Enabled, Client),
    expect_iq_success(Stanza, Client).

notifications_stanza(true, Client) ->
    #jid{lresource = Resource} = jid:from_binary(
                                   escalus_client:full_jid(Client)),
    iq_set(?NS_NOTIFICATIONS,
           #xmlel{name = <<"enable">>,
                  attrs = [{<<"device">>, Resource},
                           {<<"platform">>, <<"escalus">>}]});
notifications_stanza(false, _Client) ->
    iq_set(?NS_NOTIFICATIONS, #xmlel{name = <<"disable">>}).
