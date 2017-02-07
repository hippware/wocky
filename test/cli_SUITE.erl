%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for mod_wocky_cli
-module(cli_SUITE).
-compile(export_all).
-compile({parse_transform, fun_chain}).

-include_lib("common_test/include/ct.hrl").

-include("wocky_db_seed.hrl").
-include("wocky.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() -> [
          befriend,
          % Requires S3:
%          fix_bot_images,
          make_token
         ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    wocky_db:clear_user_tables(?LOCAL_CONTEXT),
    Users = escalus:get_users([alice, bob, carol]),
    wocky_db_seed:seed_tables(shared, [handle_to_user, bot,
                                       bot_subscriber]),
    fun_chain:first(Config,
        escalus:init_per_suite(),
        escalus:create_users(Users)
    ).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

befriend(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
      fun(Alice, Bob, Carol) ->
        {ok, _} = mod_wocky_cli:befriend(<<"alice">>, <<"bob">>),
        escalus:assert(is_roster_set, escalus:wait_for_stanza(Alice)),
        escalus:assert(is_roster_set, escalus:wait_for_stanza(Bob)),

        test_helper:ensure_all_clean([Alice, Bob, Carol])
      end).

fix_bot_images(Config) ->
    wocky_db:truncate(shared, roster),
    test_helper:set_tros_backend(s3),
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        BotFile = create_file(Alice, <<"invalid_access">>),
        ItemFile = create_file(Alice, <<"user:bob">>),
        NonBotFile = create_file(Alice, <<"invalid_access">>),

        UpdateStanza = test_helper:iq_set(
                         ?NS_BOT,
                         test_helper:node_el(?BOT, <<"fields">>,
                                             [set_image_field(BotFile)])),
        test_helper:expect_iq_success(UpdateStanza, Alice),

        %% Add bot image item
        NoteID = <<"new-item1">>,
        Content = <<"Some content">>,
        Image = tros:make_url(?LOCAL_CONTEXT, ItemFile),
        Title = <<"title ZZZ">>,
        % Alice publishes an item to her bot
        bot_SUITE:publish_item(?BOT, NoteID, Title, Content, Image, Alice),

        %% Bob's access to the images should be broken:
        tros_SUITE:download_failure(Bob, BotFile),
        tros_SUITE:download_failure(Bob, ItemFile),
        tros_SUITE:download_failure(Bob, NonBotFile),

        mod_wocky_cli:fix_bot_images(),

        %% Bob should now have access to the bot images but not the
        %% unattached one
        tros_SUITE:download_success(Bob, BotFile, file_data()),
        tros_SUITE:download_success(Bob, ItemFile, file_data()),
        tros_SUITE:download_failure(Bob, NonBotFile)
      end).

make_token(_Config) ->
    %% Just some very basic sanity tests. There's really not much
    %% to this operation that isn't tested elsewhere.
    ok = mod_wocky_cli:make_token(<<"alice">>),
    {error, _} = mod_wocky_cli:make_token(<<"non-user">>).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

create_file(Client, Access) ->
        ImageData = file_data(),
        FileSize = byte_size(ImageData),

        {QueryStanza, ResultStanza} =
        tros_SUITE:common_upload_request(FileSize, Client, Access),
        escalus:assert(is_iq_result, [QueryStanza], ResultStanza),
        tros_SUITE:do_upload(ResultStanza, ImageData, 200, false).

file_data() -> binary:copy(<<55>>, 100).

set_image_field(ID) ->
    bot_SUITE:create_field({"image", "string",
                            tros:make_url(?LOCAL_CONTEXT, ID)}).
