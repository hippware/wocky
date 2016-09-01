%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for mod_bot
-module(bot_SUITE).
-compile(export_all).

-include("wocky.hrl").
-include("wocky_bot.hrl").
-include("wocky_db_seed.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile({parse_transform, fun_chain}).
-compile({parse_transform, cut}).

-import(test_helper, [expect_iq_success/2, expect_iq_error/2]).

-define(CREATE_TITLE,       <<"Created Bot">>).
-define(CREATE_SHORTNAME,   <<"NewBot">>).
-define(CREATE_DESCRIPTION, <<"Test bot for creation operation">>).
-define(CREATE_LOCATION,    {2.5, 1.6}).
-define(CREATE_RADIUS,      10).
-define(NEW_DESCRIPTION,    <<"New bot description!">>).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [create,
     retrieve,
     update,
     affiliations,
     subscribers,
     unsubscribe,
     subscribe,
     delete,
     errors,
     update_affiliations,
     friends_only_permissions,
     roster_change_triggers,
     blocked_group
    ].

suite() ->
    escalus:suite().


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    reset_tables(Config).

end_per_suite(Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob, carol,
                                                    karen, robert, tim])),
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

reset_tables(Config) ->
    wocky_db:clear_user_tables(?LOCAL_CONTEXT),
    wocky_db:clear_tables(?LOCAL_CONTEXT, [bot, bot_name, roster,
                                           bot_subscriber]),
    wocky_db_seed:seed_tables(?LOCAL_CONTEXT, [bot, bot_name, roster,
                                               bot_subscriber]),
    Users = escalus:get_users([alice, bob, carol, karen, robert, tim]),
    Config1 = fun_chain:first(Config,
        escalus:init_per_suite(),
        escalus:create_users(Users)
    ),
    {ok, _} = wocky_db:query(shared,
                             "UPDATE user SET roster_viewers = roster_viewers "
                             "+ ? WHERE user = ? AND server = ?",
                             #{roster_viewers => [?BOT_B_JID],
                               user => ?ALICE,
                               server => ?LOCAL_CONTEXT},
                             quorum),
    Config1.


%%--------------------------------------------------------------------
%% mod_bot tests
%%--------------------------------------------------------------------

create(Config) ->
    escalus:story(Config, [{alice, 1}],
      fun(Alice) ->
        % Successfully create a bot
        Stanza = expect_iq_success(create_stanza(), Alice),
        check_returned_bot(Stanza, expected_create_fields()),

        % Fail due to shortname conflict if we try to create the same bot
        expect_iq_error(create_stanza(), Alice)
      end).

retrieve(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
      fun(Alice, Bob, Carol) ->
        % Alice can retrieve her own bot
        Stanza = expect_iq_success(retrieve_stanza(), Alice),
        check_returned_bot(Stanza, expected_retrieve_fields()),

        % Bob can retrive since he is a spectator
        Stanza2 = expect_iq_success(retrieve_stanza(), Bob),
        check_returned_bot(Stanza2, expected_retrieve_fields()),

        % Carol cannot retrive since the bot's visibiliy is WHITELIST
        % and she is neither an owner nor a spectator
        expect_iq_error(retrieve_stanza(), Carol)
      end).

update(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        % Alice can update the bot
        expect_iq_success(update_stanza(), Alice),

        % And the new bot should have the change
        Stanza = expect_iq_success(retrieve_stanza(), Alice),
        NewFields =
        lists:keyreplace("description", 1, expected_retrieve_fields(),
                         {"description", string, ?NEW_DESCRIPTION}),
        check_returned_bot(Stanza, NewFields),

        % Bob can't update it since he's not the owner
        expect_iq_error(update_stanza(), Bob)
      end).

affiliations(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        % Alice can get the correct affiliations
        Stanza = expect_iq_success(affiliations_stanza(), Alice),
        check_affiliations(Stanza, [{?ALICE_B_JID, owner},
                                    {?BOB_B_JID, spectator}]),

        % Bob can't because he's not the owner
        expect_iq_error(affiliations_stanza(), Bob)
      end).

subscribers(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        % Alice can get the correct subscribers
        Stanza = expect_iq_success(subscribers_stanza(), Alice),
        check_subscribers(Stanza, [{?CAROL_B_JID, false},
                                   {?KAREN_B_JID, true}]),

        % Bob can't because he's not the owner
        expect_iq_error(affiliations_stanza(), Bob)
      end).

unsubscribe(Config) ->
    escalus:story(Config, [{alice, 1}, {carol, 1}],
      fun(Alice, Carol) ->
        expect_iq_success(unsubscribe_stanza(), Carol),

        % Alice can get the correct subscribers
        Stanza = expect_iq_success(subscribers_stanza(), Alice),
        check_subscribers(Stanza, [{?KAREN_B_JID, true}])
      end).

subscribe(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}, {tim, 1}],
      fun(Alice, Bob, Carol, Tim) ->
        test_helper:add_contact(Alice, Tim, [], <<"Timbo">>),
        test_helper:subscribe_pair(Alice, Tim),

        timer:sleep(2000),

        NewAffiliations = [{?CAROL_B_JID, spectator}],
        expect_iq_success(modify_affiliations_stanza(NewAffiliations), Alice),
        expect_affiliation_update(Carol, spectator),

        expect_iq_success(subscribe_stanza(true), Carol),
        expect_iq_success(subscribe_stanza(false), Bob),

        % Tim can't be subscribed because he is not an affiliate and the
        % permission is WHITELIST
        expect_iq_error(subscribe_stanza(true), Tim),

        % Alice can get the correct subscribers
        Stanza = expect_iq_success(subscribers_stanza(), Alice),
        check_subscribers(Stanza, [{?KAREN_B_JID, true},
                                   {?CAROL_B_JID, true},
                                   {?BOB_B_JID, false}]),

        % Update Carol's follow status
        expect_iq_success(subscribe_stanza(false), Carol),
        Stanza2 = expect_iq_success(subscribers_stanza(), Alice),
        check_subscribers(Stanza2, [{?KAREN_B_JID, true},
                                    {?CAROL_B_JID, false},
                                    {?BOB_B_JID, false}])
      end).

delete(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        expect_iq_error(delete_stanza(), Bob),
        expect_iq_success(retrieve_stanza(), Alice),

        expect_iq_success(delete_stanza(), Alice),
        expect_iq_error(retrieve_stanza(), Alice)
      end).


errors(Config) ->
    escalus:story(Config, [{alice, 1}],
      fun(Alice) ->
        Missing = lists:keydelete("title", 1, default_fields()),
        expect_iq_error(create_stanza(Missing), Alice),

        Extra = [{"unknownfield", "string", <<"abc">>} | default_fields()],
        expect_iq_error(create_stanza(Extra), Alice),

        WrongType = [{"title", "int", 10} | Missing],
        expect_iq_error(create_stanza(WrongType), Alice)
      end).

update_affiliations(Config) ->
    reset_tables(Config),
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
      fun(Alice, Bob, Carol) ->
        NewAffiliations = [{?BOB_B_JID, none},
                           {?CAROL_B_JID, spectator}],

        test_helper:add_contact(Alice, Bob, <<"affiliates">>, <<"BMan">>),
        test_helper:add_contact(Alice, Carol, <<"affiliates">>, <<"Caz">>),

        % Let the bot's roster catch up
        timer:sleep(500),

        % Alice can modify affiliations on her contacts
        expect_iq_success(modify_affiliations_stanza(NewAffiliations), Alice),

        Stanza = expect_iq_success(affiliations_stanza(), Alice),
        check_affiliations(Stanza, [{?CAROL_B_JID, spectator},
                                    {?ALICE_B_JID, owner}]),

        % Carol and Bob get notified of their changed status
        expect_affiliation_update(Carol, spectator),
        expect_affiliation_update(Bob, none),

        %% Alice removes Bob as a contact
        escalus:send(Alice, escalus_stanza:roster_remove_contact(Bob)),
        escalus:assert_many([is_roster_set, is_iq_result],
                            escalus:wait_for_stanzas(Alice, 2)),
        escalus:assert(is_presence_with_type, [<<"unavailable">>],
                       escalus:wait_for_stanza(Bob)),

        timer:sleep(500),

        % Alice can't add a non-roster affiliate
        expect_iq_error(
          modify_affiliations_stanza([{?BOB_B_JID, spectator}]), Alice),

        % Bob can't because he's not the owner
        expect_iq_error(modify_affiliations_stanza(NewAffiliations), Bob)
      end).

friends_only_permissions(Config) ->
    reset_tables(Config),
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        expect_iq_success(
          change_visibility_stanza(?WOCKY_BOT_VIS_FRIENDS), Alice),

        test_helper:add_contact(Alice, Bob, <<"friends">>, <<"Bobbie">>),

        %% Give the bot a moment to be notified and update its copy of
        %% Alice's roster
        timer:sleep(500),

        %% Bob is a member of Alice's roster, so should be able to get the bot
        expect_iq_success(retrieve_stanza(), Bob),

        %% Alice removes Bob as a contact
        escalus:send(Alice, escalus_stanza:roster_remove_contact(Bob)),
        escalus:assert_many([is_roster_set, is_iq_result],
                            escalus:wait_for_stanzas(Alice, 2)),

        escalus:assert(is_presence_with_type, [<<"unavailable">>],
                       escalus:wait_for_stanza(Bob)),

        timer:sleep(500),

        % Bob was an affiliate at this point, so gets a notification that
        % he has been de-affiliated
        expect_affiliation_update(Bob, none),

        % Bob can't get the bot because he's no longer a contact
        expect_iq_error(retrieve_stanza(), Bob),

        %% Alice adds Bob back as a contact
        test_helper:add_contact(Alice, Bob, <<"friends">>, <<"Bobbie">>),
        test_helper:subscribe_pair(Alice, Bob),

        timer:sleep(500),

        %% Bob is a member of Alice's roster, so should be able to get the bot
        expect_iq_success(retrieve_stanza(), Bob),

        %% Set the bot back to WHITELIST
        expect_iq_success(
          change_visibility_stanza(?WOCKY_BOT_VIS_WHITELIST), Alice),

        %% Alice removes Bob as a contact so that subsequent tests don't fail
        escalus:send(Alice, escalus_stanza:roster_remove_contact(Bob)),
        escalus:assert_many([is_roster_set, is_iq_result,
                            escalus_pred:is_presence_with_type(
                              <<"unavailable">>, _)],
                            escalus:wait_for_stanzas(Alice, 3)),

        escalus:assert_many([is_roster_set,
                             escalus_pred:is_presence_with_type(
                               <<"unsubscribe">>, _)],
                       escalus:wait_for_stanzas(Bob, 2))
      end).

roster_change_triggers(Config) ->
    reset_tables(Config),
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1},
                           {karen, 1}, {robert, 1}],
      fun(Alice, Bob, Carol, Karen, Robert) ->
        IsPresUnavailable =
        fun(S) ->
                escalus_pred:is_presence_with_type(<<"unavailable">>, S)
        end,

        % Robert is nothin', so gets no bot notifications
        escalus:send(Alice, escalus_stanza:roster_remove_contact(Robert)),
        escalus:assert_many([is_roster_set, is_iq_result],
                            escalus:wait_for_stanzas(Alice, 2)),

        escalus:assert(IsPresUnavailable, escalus:wait_for_stanza(Robert)),

        % Bob is an affiliate but not subscriber
        escalus:send(Alice, escalus_stanza:roster_remove_contact(Bob)),
        escalus:assert_many([is_roster_set, is_iq_result],
                            escalus:wait_for_stanzas(Alice, 2)),

        escalus:assert_many([IsPresUnavailable,
                             is_affiliation_update(
                               escalus_client:short_jid(Bob), none, _)],
                            escalus:wait_for_stanzas(Bob, 2)),

        % Carol is a subscriber but not affiliate
        escalus:send(Alice, escalus_stanza:roster_remove_contact(Carol)),
        escalus:assert_many([is_roster_set, is_iq_result],
                            escalus:wait_for_stanzas(Alice, 2)),

        escalus:assert_many([IsPresUnavailable,
                             is_bot_unsubscribe(false, _)],
                            escalus:wait_for_stanzas(Carol, 2)),

        % Carol is both a subscriber and affiliate (after we affiliate and
        % subscribe her)
        expect_iq_success(modify_affiliations_stanza(
                            [{?KAREN_B_JID, spectator}]), Alice),
        escalus:assert(is_affiliation_update(escalus_client:short_jid(Karen),
                                             spectator, _),
                       escalus:wait_for_stanza(Karen)),
        expect_iq_success(subscribe_stanza(true), Karen),

        escalus:send(Alice, escalus_stanza:roster_remove_contact(Karen)),
        escalus:assert_many([is_roster_set, is_iq_result],
                            escalus:wait_for_stanzas(Alice, 2)),

        escalus:assert_many([IsPresUnavailable,
                             is_affiliation_update(
                               escalus_client:short_jid(Karen), none, _),
                             is_bot_unsubscribe(true, _)],
                            escalus:wait_for_stanzas(Karen, 3)),

        test_helper:ensure_all_clean([Alice, Bob, Carol, Karen, Robert])
      end).

blocked_group(Config) ->
    reset_tables(Config),
    escalus:story(Config, [{alice, 1}, {tim, 1}],
      fun(Alice, Tim) ->
        expect_iq_success(
          change_visibility_stanza(?WOCKY_BOT_VIS_FRIENDS), Alice),

        % Alice adds Tim as a normal friend
        test_helper:add_contact(Alice, Tim, <<"blah">>,
                                <<"He's okay">>),
        test_helper:subscribe_pair(Alice, Tim),
        timer:sleep(500),

        % Tim can view the bot
        expect_iq_success(retrieve_stanza(), Tim),

        % Alice moves Tim to the magic __blocked__ group
        test_helper:add_contact(Alice, Tim, <<"__blocked__">>,
                                <<"MyMortalEnemy">>),
        escalus:assert(is_presence_with_type, [<<"unavailable">>],
                       escalus:wait_for_stanza(Tim)),
        timer:sleep(500),

        % Tim can no longer view the bot
        expect_iq_error(retrieve_stanza(), Tim)
      end).


%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

create_stanza() ->
    create_stanza(default_fields()).
create_stanza(Fields) ->
    test_helper:iq_set(?NS_BOT,
                       #xmlel{name = <<"create">>,
                              children = create_fields(Fields)}).

default_fields() ->
    [{"title",         "string", ?CREATE_TITLE},
     {"shortname",     "string", ?CREATE_SHORTNAME},
     {"description",   "string", ?CREATE_DESCRIPTION},
     {"location",      "geoloc", ?CREATE_LOCATION},
     {"radius",        "int",    ?CREATE_RADIUS}
    ].

create_fields(Fields) ->
    [create_field(F) || F <- Fields].

create_field({Name, "string", Value}) ->
    create_field(Name, "string", value_element(Value));
create_field({Name, "int", Value}) ->
    create_field(Name, "int", value_element(integer_to_binary(Value)));
create_field({Name, "jid", Value}) ->
    create_field(Name, "jid", value_element(jid:to_binary(Value)));
create_field({Name, "geoloc", Value}) ->
    create_field(Name, "geoloc", geoloc_element(Value)).

value_element(Value) ->
    #xmlel{name = <<"value">>, children = [#xmlcdata{content = Value}]}.

geoloc_element({Lat, Lon}) ->
    #xmlel{name = <<"geoloc">>,
           attrs = [{<<"xmlns">>, ?NS_GEOLOC}],
           children = [coordinate_element(<<"lat">>, Lat),
                       coordinate_element(<<"lon">>, Lon)]}.

coordinate_element(Name, Val) ->
    #xmlel{name = Name,
           children = [#xmlcdata{content = float_to_binary(Val)}]}.

create_field(Name, Type, Child) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"var">>, list_to_binary(Name)},
                    {<<"type">>, list_to_binary(Type)}],
           children = [Child]}.

expected_create_fields() ->
    [{"id", string, any},
     {"server", string, ?LOCAL_CONTEXT},
     {"title", string, ?CREATE_TITLE},
     {"shortname", string, ?CREATE_SHORTNAME},
     {"owner", jid, ?ALICE_B_JID},
     {"description", string, ?CREATE_DESCRIPTION},
     {"location", geoloc, ?CREATE_LOCATION},
     {"radius", int, ?CREATE_RADIUS},
     {"visibility", int, ?WOCKY_BOT_VIS_OWNER},
     {"alerts", int, ?WOCKY_BOT_ALERT_DISABLED},
     {"jid", jid, any},
     {"affiliates+size", int, 1}, % Owner is always an affiliate
     {"affiliates+hash", string, any},
     {"subscribers+size", int, 0},
     {"subscribers+hash", string, any}].

expected_retrieve_fields() ->
    [{"id", string, ?BOT},
     {"server", string, ?LOCAL_CONTEXT},
     {"title", string, ?BOT_TITLE},
     {"shortname", string, ?BOT_NAME},
     {"owner", jid, ?ALICE_B_JID},
     {"description", string, ?BOT_DESC},
     {"location", geoloc, {?BOT_LAT, ?BOT_LON}},
     {"radius", int, ?BOT_RADIUS},
     {"visibility", int, ?WOCKY_BOT_VIS_WHITELIST},
     {"alerts", int, ?WOCKY_BOT_ALERT_DISABLED},
     {"jid", jid, bot_jid(?BOT)},
     {"affiliates+size", int, 2}, % Owner is always an affiliate
     {"affiliates+hash", string, any},
     {"subscribers+size", int, 2},
     {"subscribers+hash", string, any}].

check_returned_bot(#xmlel{name = <<"iq">>, children = [BotStanza]},
                   ExpectedFields) ->
    #xmlel{name = <<"bot">>, attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = Children} = BotStanza,
    check_return_fields(Children, ExpectedFields).

check_return_fields(Elements, ExpectedFields) ->
    lists:foreach(check_field(_, Elements), ExpectedFields).

check_field({Name, Type, any}, Elements) ->
    ?assert(has_field(list_to_binary(Name), atom_to_binary(Type, utf8),
            Elements));
check_field({Name, Type, Value}, Elements) ->
    ?assert(has_field_val(list_to_binary(Name), atom_to_binary(Type, utf8),
                          Value, Elements)).

has_field(Name, Type, Elements) ->
    ct:log("Checking field: ~p ~p", [Name, Type]),
    find_field(Name, Type, Elements) =/= false.

has_field_val(Name, Type, Value, Elements) ->
    ct:log("Checking field: ~p ~p ~p", [Name, Type, Value]),
    case find_field(Name, Type, Elements) of
        #xmlel{name = <<"field">>, children = [ValueEl]} ->
            check_value_el(Value, Type, ValueEl);
        X ->
            ct:fail("find_field ~p ~p ~p returned ~p",
                    [Name, Type, Elements, X])
    end.

check_value_el(Value, <<"geoloc">>, El = #xmlel{name = <<"geoloc">>}) ->
    check_geoloc_val(Value, El);
check_value_el(Value, Type,
               #xmlel{name = <<"value">>,
                      children = [#xmlcdata{content = Value}]})
    when Type =:= <<"string">> orelse Type =:= <<"jid">> ->
    true;
check_value_el(Value, <<"int">>,
               #xmlel{name = <<"value">>,
                      children = [#xmlcdata{content = InValue}]}) ->
    Value =:= binary_to_integer(InValue);
check_value_el(Value, Type, Element) ->
    ct:fail("check_value_el failed: ~p ~p ~p", [Value, Type, Element]).

check_geoloc_val({Lat, Lon}, #xmlel{name = <<"geoloc">>,
                                    attrs = Attrs,
                                    children = Children}) ->
    ?assertEqual({value, ?NS_GEOLOC}, xml:get_attr(<<"xmlns">>, Attrs)),
    check_child(<<"lat">>, Lat, Children),
    check_child(<<"lon">>, Lon, Children).

check_child(Name, Value, Children) ->
    case lists:keyfind(Name, #xmlel.name, Children) of
        false ->
            ct:fail("Could not find ~p geoloc element", [Name]);
        #xmlel{children = [#xmlcdata{content = InVal}]} ->
            Value == binary_to_float(InVal);
        _ ->
            ct:fail("Wrong value for ~p - expected ~p)", [Name, Value])
    end.

find_field(Name, Type, Elements) ->
    case lists:dropwhile(fun(E) -> not is_field(Name, Type, E) end, Elements) of
        [] -> false;
        List -> hd(List)
    end.

is_field(Name, Type, #xmlel{attrs = Attrs}) ->
    xml:get_attr(<<"var">>, Attrs) =:= {value, Name} andalso
    xml:get_attr(<<"type">>, Attrs) =:= {value, Type}.

retrieve_stanza() ->
    test_helper:iq_get(?NS_BOT, node_el(<<"bot">>)).

node_el(Name) -> node_el(Name, []).
node_el(Name, Children) ->
    #xmlel{name = Name, attrs = [{<<"node">>, bot_node(?BOT)}],
           children = Children}.

bot_node(ID) ->
    <<"bot/", ID/binary>>.

bot_jid(ID) ->
    jid:to_binary(jid:make(<<>>, ?LOCAL_CONTEXT, bot_node(ID))).

change_visibility_stanza(Visibility) ->
    test_helper:iq_set(?NS_BOT, node_el(<<"fields">>,
                                        visibility_field(Visibility))).

visibility_field(Visibility) ->
    create_field({"visibility", "int", Visibility}).

update_stanza() ->
    test_helper:iq_set(?NS_BOT, node_el(<<"fields">>, modify_field())).

modify_field() ->
    create_field({"description", "string", ?NEW_DESCRIPTION}).

affiliations_stanza() ->
    test_helper:iq_get(?NS_BOT, node_el(<<"affiliations">>)).

check_affiliations(#xmlel{name = <<"iq">>, children = [AffiliationsEl]},
                   Affiliates) ->
    Affiliations = AffiliationsEl#xmlel.children,
    ?assertEqual({value, integer_to_binary(length(Affiliates))},
                 xml:get_attr(<<"size">>, AffiliationsEl#xmlel.attrs)),
    ?assertEqual(length(Affiliates), length(Affiliations)),
    lists:foreach(check_affiliation(Affiliations, _), Affiliates).

check_affiliation(AffiliationEls, Affiliate) ->
    case lists:any(is_affiliate(Affiliate, _), AffiliationEls) of
        true -> true;
        false -> ct:fail("Missing affiliate ~p", [Affiliate])
    end.

is_affiliate({Name, Type}, #xmlel{name = <<"affiliation">>, attrs = Attrs}) ->
    xml:get_attr(<<"jid">>, Attrs) =:= {value, Name} andalso
    xml:get_attr(<<"affiliation">>, Attrs) =:=
     {value, atom_to_binary(Type, utf8)}.

subscribers_stanza() ->
    test_helper:iq_get(?NS_BOT, node_el(<<"subscribers">>)).

check_subscribers(#xmlel{name = <<"iq">>, children = [SubscribersEl]},
                   Subscribers) ->
    ReceivedSubscribers = SubscribersEl#xmlel.children,
    ?assertEqual({value, integer_to_binary(length(Subscribers))},
                 xml:get_attr(<<"size">>, SubscribersEl#xmlel.attrs)),
    ?assertEqual(length(Subscribers), length(ReceivedSubscribers)),
    lists:foreach(check_subscriber(ReceivedSubscribers, _), Subscribers).

check_subscriber(SubscriberEls, {Name, Follow}) ->
    case lists:dropwhile(fun(El) -> not is_subscriber(Name, El) end,
                         SubscriberEls) of
        [] -> ct:fail("Missing subscriber ~p", [Name]);
        L -> check_follow(hd(L), Follow)
    end.

is_subscriber(Name, #xmlel{name = <<"subscriber">>, attrs = Attrs}) ->
    xml:get_attr(<<"jid">>, Attrs) =:= {value, Name}.

check_follow(El, Follow) ->
    ct:log("Checking follow ~p ~p", [El, Follow]),
    case xml:get_path_s(El, [{elem, <<"follow">>}, cdata]) of
        <<"0">> -> ?assertNot(Follow);
        <<"1">> -> ?assert(Follow);
        X -> ct:fail("Invalid or missing <follow> element: ~p", [X])
    end.

modify_affiliations_stanza(NewAffiliations) ->
    test_helper:iq_set(?NS_BOT,
                       node_el(<<"affiliations">>,
                               affiliation_els(NewAffiliations))).

affiliation_els(Affiliations) ->
    [affiliation_el(JID, Role) || {JID, Role} <- Affiliations].

affiliation_el(JID, Role) ->
    #xmlel{name = <<"affiliation">>,
           attrs = [{<<"jid">>, JID},
                    {<<"affiliation">>, atom_to_binary(Role, utf8)}]}.

expect_affiliation_update(Client, Type) ->
    Stanza = escalus:wait_for_stanza(Client),
    escalus:assert(
      is_affiliation_update(escalus_client:short_jid(Client), Type, _), Stanza).

is_affiliation_update(Name, Type,
                      #xmlel{name = <<"message">>,
                                children = [AffiliationsEl]}) ->
    is_affiliation_element(Name, Type, AffiliationsEl);
is_affiliation_update(_, _, _) ->false.

is_affiliation_element(Name, Type,
                       #xmlel{name = <<"affiliations">>, attrs = Attrs,
                              children = [AffiliateEl]}) ->
    has_standard_attrs(Attrs) andalso
    is_affiliate({Name, Type}, AffiliateEl);
is_affiliation_element(_, _, _) ->false.

is_bot_unsubscribe(OldFollow,
                   #xmlel{name = <<"message">>, children = [Unsubscribed]}) ->
    Attrs = Unsubscribed#xmlel.attrs,
    <<"unsubscribed">> =:= Unsubscribed#xmlel.name andalso
    has_standard_attrs(Attrs) andalso
    follow_cdata(OldFollow) =:= xml:get_path_s(Unsubscribed,
                                               [{elem, <<"follow">>}, cdata]);
is_bot_unsubscribe(_, _) -> false.

has_standard_attrs(Attrs) ->
    {value, bot_node(?BOT)} =:= xml:get_attr(<<"node">>, Attrs)  andalso
    {value, ?NS_BOT} =:= xml:get_attr(<<"xmlns">>, Attrs).

unsubscribe_stanza() ->
    test_helper:iq_set(?NS_BOT, node_el(<<"unsubscribe">>)).

subscribe_stanza(Follow) ->
    SubEl = node_el(<<"subscribe">>),
    FullSubEl = SubEl#xmlel{children = [follow_el(Follow)]},
    test_helper:iq_set(?NS_BOT, FullSubEl).

follow_el(Follow) ->
    #xmlel{name = <<"follow">>,
           children = [#xmlcdata{content = follow_cdata(Follow)}]}.

follow_cdata(false) -> <<"0">>;
follow_cdata(true) -> <<"1">>.

delete_stanza() ->
    test_helper:iq_set(?NS_BOT, node_el(<<"delete">>)).

