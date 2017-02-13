%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for mod_wocky_cli
-module(cli_SUITE).
-compile(export_all).
-compile({parse_transform, fun_chain}).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("wocky_db_seed.hrl").
-include("wocky.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() -> [
          befriend,
          make_token,
% Requires S3:
          fix_bot_images,
          migrate_metadata_story
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
        BotFile = create_file(Config, Alice, <<"invalid_access">>),
        ItemFile = create_file(Config, Alice, <<"user:bob">>),
        NonBotFile = create_file(Config, Alice, <<"invalid_access">>),

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
        tros_SUITE:download_success(Bob, BotFile, out_file_data(Config)),
        tros_SUITE:download_success(Bob, ItemFile, out_file_data(Config)),
        tros_SUITE:download_failure(Bob, NonBotFile)
      end).

migrate_metadata_story(_Config) ->
    wocky_db:truncate(shared, file_metadata),
    test_helper:set_tros_backend(s3),
    File1 = wocky_db:create_id(),
    File2 = wocky_db:create_id(),
    File3 = wocky_db:create_id(),

    seed_s3_file(?ALICE_JID, File1),
    seed_s3_file(?ALICE_JID, File2),
    % Upload File3 with the new system to simulate a file that's already been
    % migrated
    wocky_db_seed:maybe_seed_s3_file(?ALICE_JID, File3),

    ok = mod_wocky_tros_s3_legacy:update_access(?LOCAL_CONTEXT,
                                                File1, <<"test_access">>),

    ?assertEqual(not_found, wocky_db_tros:get_access(?LOCAL_CONTEXT, File1)),
    ?assertEqual(not_found, wocky_db_tros:get_access(?LOCAL_CONTEXT, File2)),
    ?assertEqual(<<"all">>, wocky_db_tros:get_access(?LOCAL_CONTEXT, File3)),
    ?assertEqual(?ALICE, wocky_db_tros:get_owner(?LOCAL_CONTEXT, File3)),

    mod_wocky_cli:tros_migrate_access(),

    ?assertEqual(?ALICE, wocky_db_tros:get_owner(?LOCAL_CONTEXT, File1)),
    ?assertEqual(?ALICE, wocky_db_tros:get_owner(?LOCAL_CONTEXT, File2)),
    ?assertEqual(<<"test_access">>,
                 wocky_db_tros:get_access(?LOCAL_CONTEXT, File1)),
    ?assertEqual(<<"all">>, wocky_db_tros:get_access(?LOCAL_CONTEXT, File2)),
    % File3 should be unchanged
    ?assertEqual(<<"all">>, wocky_db_tros:get_access(?LOCAL_CONTEXT, File3)),
    ?assertEqual(?ALICE, wocky_db_tros:get_owner(?LOCAL_CONTEXT, File3)).


make_token(_Config) ->
    %% Just some very basic sanity tests. There's really not much
    %% to this operation that isn't tested elsewhere.
    ok = mod_wocky_cli:make_token(<<"alice">>),
    {error, _} = mod_wocky_cli:make_token(<<"non-user">>).

seed_s3_file(UserJID, FileID) ->
    {Headers, Fields} = mod_wocky_tros_s3_legacy:make_upload_response(
                          UserJID, #jid{lserver = ?LOCAL_CONTEXT},
                          FileID, 1000,
                          <<"all">>, #{<<"content-type">> => <<"image/png">>}),
    HeadersStr = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Headers],
    {ok, _} =
    httpc:request(put,
                  {binary_to_list(proplists:get_value(<<"url">>, Fields)),
                   HeadersStr, "image/png", crypto:strong_rand_bytes(1000)},
                  [], []).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

create_file(Config, Client, Access) ->
        ImageData = load_file(Config, "in.png"),
        FileSize = byte_size(ImageData),

        {QueryStanza, ResultStanza} =
        tros_SUITE:common_upload_request(FileSize, Client, Access),
        escalus:assert(is_iq_result, [QueryStanza], ResultStanza),
        tros_SUITE:do_upload(ResultStanza, ImageData, 200, false).

out_file_data(Config) -> load_file(Config, "out.jpg").

set_image_field(ID) ->
    bot_SUITE:create_field({"image", "string",
                            tros:make_url(?LOCAL_CONTEXT, ID)}).

load_file(Config, FileName) ->
    DataDir = proplists:get_value(data_dir, Config),
    ImageFile = filename:join(DataDir, FileName),
    {ok, ImageData} = file:read_file(ImageFile),
    ImageData.
