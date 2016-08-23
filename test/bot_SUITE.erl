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
     update_affiliations,
     followers,
     unfollow,
     follow,
     delete
    ].

suite() ->
    escalus:suite().


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    wocky_db:clear_user_tables(?LOCAL_CONTEXT),
    wocky_db:clear_tables(?LOCAL_CONTEXT, [bot, bot_name, roster]),
    wocky_db_seed:seed_tables(?LOCAL_CONTEXT, [bot, bot_name, roster]),
    Users = escalus:get_users([alice, bob, carol, karen]),
    fun_chain:first(Config,
        escalus:init_per_suite(),
        escalus:create_users(Users)
    ).

end_per_suite(Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob, carol, karen])),
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

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
        check_affiliations(Stanza, [{?BOB_B_JID, spectator},
                                    {?ALICE_B_JID, owner}]),

        % Bob can't because he's not the owner
        expect_iq_error(affiliations_stanza(), Bob)
      end).

update_affiliations(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
      fun(Alice, Bob, Carol) ->
        NewAffiliations = [{?BOB_B_JID, none},
                           {?CAROL_B_JID, spectator}],

        % Alice can modify affiliations
        expect_iq_success(modify_affiliations_stanza(NewAffiliations), Alice),

        Stanza = expect_iq_success(affiliations_stanza(), Alice),
        check_affiliations(Stanza, [{?CAROL_B_JID, spectator},
                                    {?ALICE_B_JID, owner}]),

        % Carol and Bob get notified of their changed status
        expect_affiliation_update(Carol, spectator),
        expect_affiliation_update(Bob, none),

        % Bob can't because he's not the owner
        expect_iq_error(modify_affiliations_stanza(NewAffiliations), Bob)
      end).

followers(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        % Alice can get the correct followers
        Stanza = expect_iq_success(followers_stanza(), Alice),
        check_followers(Stanza, [?CAROL_B_JID]),

        % Bob can't because he's not the owner
        expect_iq_error(affiliations_stanza(), Bob)
      end).

unfollow(Config) ->
    escalus:story(Config, [{alice, 1}, {carol, 1}],
      fun(Alice, Carol) ->
        expect_iq_success(unfollow_stanza(), Carol),

        % Alice can get the correct followers
        Stanza = expect_iq_success(followers_stanza(), Alice),
        check_followers(Stanza, [])
      end).

follow(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {karen, 1}],
      fun(Alice, Bob, Karen) ->
        NewAffiliations = [{?KAREN_B_JID, spectator}],
        expect_iq_success(modify_affiliations_stanza(NewAffiliations), Alice),
        expect_affiliation_update(Karen, spectator),
        expect_iq_success(follow_stanza(), Karen),

        expect_iq_error(follow_stanza(), Bob),

        % Alice can get the correct followers
        Stanza = expect_iq_success(followers_stanza(), Alice),
        check_followers(Stanza, [?KAREN_B_JID])
      end).

delete(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        expect_iq_error(delete_stanza(), Bob),
        expect_iq_success(retrieve_stanza(), Alice),

        expect_iq_success(delete_stanza(), Alice),
        expect_iq_error(retrieve_stanza(), Alice)
      end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

create_stanza() ->
    test_helper:iq_set(?NS_BOT,
                       #xmlel{name = <<"create">>,
                              children = create_fields()}).

create_fields() ->
    Fields = [{"title",         "string", ?CREATE_TITLE},
              {"shortname",     "string", ?CREATE_SHORTNAME},
              {"description",   "string", ?CREATE_DESCRIPTION},
              {"location",      "geoloc", ?CREATE_LOCATION},
              {"radius",        "int",    ?CREATE_RADIUS}
             ],
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
     {"followers+size", int, 0},
     {"followers+hash", string, any}].

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
     {"followers+size", int, 1},
     {"followers+hash", string, any}].

check_returned_bot(#xmlel{name = <<"iq">>, children = [BotStanza]},
                   ExpectedFields) ->
    #xmlel{name = <<"bot">>, attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = Children} = BotStanza,
    check_return_fields(Children, ExpectedFields).

check_return_fields(Elements, ExpectedFields) ->
    lists:foreach(check_field(_, Elements), ExpectedFields).

check_field({Name, Type, any}, Elements) ->
    ct:log("Checking for field ~p ~p", [Name, Type]),
    ?assert(has_field(list_to_binary(Name), atom_to_binary(Type, utf8),
            Elements));
check_field({Name, Type, Value}, Elements) ->
    ct:log("Checking for field ~p ~p ~p", [Name, Type, Value]),
    ?assert(has_field_val(list_to_binary(Name), atom_to_binary(Type, utf8),
                          Value, Elements)).

has_field(Name, Type, Elements) ->
    find_field(Name, Type, Elements) =/= false.

has_field_val(Name, Type, Value, Elements) ->
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

followers_stanza() ->
    test_helper:iq_get(?NS_BOT, node_el(<<"followers">>)).

check_followers(#xmlel{name = <<"iq">>, children = [FollowersEl]},
                   Followers) ->
    ReceivedFollowers = FollowersEl#xmlel.children,
    ?assertEqual({value, integer_to_binary(length(Followers))},
                 xml:get_attr(<<"size">>, FollowersEl#xmlel.attrs)),
    ?assertEqual(length(Followers), length(ReceivedFollowers)),
    lists:foreach(check_follower(ReceivedFollowers, _), Followers).

check_follower(FollowerEls, Follower) ->
    case lists:any(is_follower(Follower, _), FollowerEls) of
        true -> true;
        false -> ct:fail("Missing follower ~p", [Follower])
    end.

is_follower(Name, #xmlel{name = <<"follower">>, attrs = Attrs}) ->
    xml:get_attr(<<"jid">>, Attrs) =:= {value, Name}.

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
    check_affiliation_update(Stanza, escalus_client:short_jid(Client), Type).

check_affiliation_update(#xmlel{name = <<"message">>,
                                children = [AffiliationsEl]},
                         Name, Type) ->
    #xmlel{name = <<"affiliations">>, attrs = Attrs,
           children = [AffiliateEl]} = AffiliationsEl,
    ?assertEqual(xml:get_attr(<<"xmlns">>, Attrs), {value, ?NS_BOT}),
    ?assertEqual(xml:get_attr(<<"node">>, Attrs),
                 {value, bot_node(?BOT)}),
    ?assert(is_affiliate({Name, Type}, AffiliateEl)).

unfollow_stanza() ->
    test_helper:iq_set(?NS_BOT, node_el(<<"unfollow">>)).

follow_stanza() ->
    test_helper:iq_set(?NS_BOT, node_el(<<"follow">>)).

delete_stanza() ->
    test_helper:iq_set(?NS_BOT, node_el(<<"delete">>)).

