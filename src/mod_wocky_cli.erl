%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing Wocky CLI commands
%%%
-module(mod_wocky_cli).

-behaviour(gen_mod).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).
-compile({parse_transform, fun_chain}).

-include_lib("ejabberd/include/ejabberd_commands.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include("wocky_roster.hrl").

-define(s3, 'Elixir.ExAws.S3').
-define(ex_aws, 'Elixir.ExAws').
-define(enum, 'Elixir.Enum').

%% gen_mod handlers
-export([start/2, stop/1]).

%% commands
-export([befriend/2,
         tros_migrate/0,
         tros_cleanup/0,
         tros_migrate_access/0,
         fix_bot_images/0,
         make_token/1,
         reprocess_images/0
        ]).

-ignore_xref([befriend/2,
              tros_migrate/0,
              tros_cleanup/0,
              tros_migrate_access/0,
              fix_bot_images/0,
              make_token/1,
              reprocess_images/0
             ]).

%%%===================================================================
%%% gen_mod handlers
%%%===================================================================

start(_Host, _Opts) ->
    ejabberd_commands:register_commands(commands()).

stop(_Host) ->
    ejabberd_commands:unregister_commands(commands()).

commands() ->
    [
     %% Mandatory friendship happy fun times
     #ejabberd_commands{name     = befriend,
                        desc     = "Make two users friends",
                        module   = ?MODULE,
                        function = befriend,
                        args     = [{user1, binary}, {user2, binary}],
                        result   = {result, restuple}},

     %% TROS migration
     #ejabberd_commands{name     = tros_migrate,
                        desc     = "Migrate TROS data from francus to S3",
                        module   = ?MODULE,
                        function = tros_migrate,
                        args     = [],
                        result   = {result, rescode}},
     #ejabberd_commands{name     = tros_cleanup,
                        desc     = "Delete TROS data from Francus",
                        module   = ?MODULE,
                        function = tros_cleanup,
                        args     = [],
                        result   = {result, rescode}},

     #ejabberd_commands{name     = tros_migrate_access,
                        desc     = "Migrate TROS access data "
                                   "from S3 back to the local DB",
                        longdesc = "Output:\n"
                                   ". : Metadata successfully migrated\n"
                                   "- : Metadata already in DB;"
                                   " no action taken\n"
                                   "N : No metadata found in S3 - probably a"
                                   " newly created file",
                        module   = ?MODULE,
                        function = tros_migrate_access,
                        args     = [],
                        result   = {result, rescode}},

     %% Traffic dumping
     #ejabberd_commands{name     = dump_traffic,
                        desc     = "Dump traffic for a specified user",
                        longdesc = "Parameters: <user> <start> <duration>\n"
                                   "<user> The handle for the user\n"
                                   "<start> Start time for dump (ISO format) "
                                   "eg 2016-05-20T23:45:00Z\n"
                                   "<duration> [Period][h|m|s|ms] eg: 50s",
                        module   = traffic_dumper,
                        function = dump,
                        args     = [{user, binary},
                                    {start, binary},
                                    {duration, binary}],
                        result   = {result, rescode}},
     #ejabberd_commands{name     = dump_traffic_r,
                        desc     = "Dump traffic for a specified user/resource",
                        longdesc = "Parameters: <user> <resource> "
                                   "<start> <duration>\n"
                                   "<user> The handle for the user\n"
                                   "<resource> The user's resource\n"
                                   "<start> Start time for dump (ISO format) "
                                   "eg 2016-05-20T23:45:00Z\n"
                                   "<duration> [Period][h|m|s|ms] eg: 50s",
                        module   = traffic_dumper,
                        function = dump,
                        args     = [{user, binary},
                                    {resource, binary},
                                    {start, binary},
                                    {duration, binary}],
                        result   = {result, rescode}},

     %% Fix bot image permissions
     #ejabberd_commands{name     = fix_bot_images,
                        desc     = "Fix invalid permissions on bot images",
                        longdesc = "Parameters: none\n",
                        module   = ?MODULE,
                        function = fix_bot_images,
                        args     = [],
                        result   = {result, rescode}},

     %% Manual token generation
     #ejabberd_commands{name     = make_token,
                        desc     = "Generate an auth token",
                        longdesc = "Parameters: <user>\n",
                        module   = ?MODULE,
                        function = make_token,
                        args     = [{user, binary}],
                        result   = {result, rescode}},

     %% Bot report generator
     #ejabberd_commands{name     = bot_report,
                        desc     = "Generate report on bots",
                        longdesc = "Parameters: <days>\n",
                        module   = wocky_report,
                        function = generate_bot_report,
                        args     = [{duration, integer}],
                        result   = {result, rescode}},

     %% TROS image reprocessor
     #ejabberd_commands{name     = reprocess_images,
                        desc     = "Reprocess all image thumbnails and full "
                                   "versions from source image",
                        module   = ?MODULE,
                        function = reprocess_images,
                        args     = [],
                        result   = {result, rescode}}

    ].

%%%===================================================================
%%% Command implementation - befriend
%%%===================================================================

befriend(Handle1, Handle2) ->
    do([error_m ||
        User1 <- get_user(Handle1),
        User2 <- get_user(Handle2),
        make_friends(User1, User2),
        {ok, "Success"}
       ]).

make_friends(User1, User2) ->
    lists:foreach(
      make_friend(_), [{User1, User2}, {User2, User1}]).

make_friend({#{user := User1, server := Server1},
             #{user := User2, server := Server2}}) ->
    JID2 = jid:to_binary(jid:make(User2, Server2, <<>>)),
    RosterItem = wocky_db_roster:get_roster_item(User1, Server1, JID2),
    RosterItem2 = RosterItem#wocky_roster{subscription = both,
                                          ask = none},
    wocky_db_roster:update_roster_item(User1, Server1, JID2, RosterItem2),
    ejabberd_hooks:run(roster_modified, wocky_app:server(),
                       [User1, Server1, JID2]).

%%%===================================================================
%%% Command implementation - tros_migrate
%%%===================================================================

tros_migrate() ->
    Files = get_files(),
    migrate_files(Files),
    verify_files(Files),
    ok.

tros_cleanup() ->
    cleanup_files(get_files()),
    ok.

get_files() ->
    Files = wocky_db:select_column(wocky_app:server(), media, id, #{}),
    io:fwrite("Found ~p files to migrate\n", [length(Files)]),
    Files.

migrate_files(Files) ->
    io:fwrite("Migrating:\n"),
    lists:foldl(migrate_file(_, _, length(Files)), 1, Files),
    io:fwrite("\n").

migrate_file(File, Count, Total) ->
    {Data, Owner, Access, Metadata, Size} = read_file(File),

    {Headers, RespFields} =
    mod_wocky_tros_s3:make_upload_response(
      #jid{luser = Owner}, #jid{lserver = wocky_app:server()},
      File, Size, Access, Metadata),
    Method = list_to_atom(
               string:to_lower(
                 binary_to_list(
                   proplists:get_value(<<"method">>, RespFields)))),
    URL = binary_to_list(proplists:get_value(<<"url">>, RespFields)),
    HeadersStr = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Headers],
    ContentType = proplists:get_value("content-type", HeadersStr),
    {ok, _} = httpc:request(Method,
                            {URL, HeadersStr, ContentType, Data},
                            [], []),

    print_progress(Count, Total),
    Count+1.

read_file(File) ->
    {ok, F1} = francus:open_read(wocky_app:server(), File),
    {F2, Data} = francus:read(F1),
    Owner = francus:owner(F2),
    Access = francus:access(F2),
    Metadata = francus:metadata(F2),
    Size = francus:size(F2),
    francus:close(F2),
    {Data, Owner, Access, Metadata, Size}.


print_progress(Count, _Total) when Count rem 10 =:= 0 ->
    io:fwrite("~p", [Count]);
print_progress(_Count, _Total) ->
    io:fwrite(".").

verify_files(Files) ->
    io:fwrite("Verifying:\n"),
    lists:foldl(verify_file(_, _, length(Files)), 1, Files),
    io:fwrite("\n").

verify_file(File, Count, Total) ->
    {Data, Owner, Access, _Metadata, _Size} = read_file(File),
    {_, RespFields} = mod_wocky_tros_s3:make_download_response(
                        unused, #jid{lserver = wocky_app:server()},
                        unused, File, unused),

    URL = binary_to_list(proplists:get_value(<<"url">>, RespFields)),
    {ok, {_, _, Body}} = httpc:request(get, {URL, []}, [],
                                       [{body_format, binary}]),
    check_equal(Data, Body),

    {ok, S3Metadata} = mod_wocky_tros_s3:get_metadata(wocky_app:server(), File),
    check_equal({ok, Owner}, mod_wocky_tros_s3:get_owner(S3Metadata)),
    check_equal({ok, Access}, mod_wocky_tros_s3:get_access(S3Metadata)),

    print_progress(Count, Total),
    Count+1.

check_equal(A, A) -> ok;
check_equal(A, B) ->
    io:fwrite("Mismatch. Expected:\n~p\nGot:\n~p\n", [A, B]),
    erlang:error("Data mismatch - aborting").

cleanup_files(Files) ->
    io:fwrite("Cleaning up francus files:\n"),
    lists:foldl(cleanup_file(_, _, length(Files)), 1, Files),
    io:fwrite("\n").

cleanup_file(File, Count, Total) ->
    francus:delete(wocky_app:server(), File),
    print_progress(Count, Total),
    Count+1.

%%%===================================================================
%%% Command implementation - tros_migrate_access
%%%===================================================================

tros_migrate_access() ->
    fun_chain:first(
      mod_wocky_tros_s3:bucket(),
      ?s3:list_objects(),
      ?ex_aws:'stream!'(s3_auth()),
      ?enum:to_list(),
      migrate_access()
     ),
    ok.

migrate_access(Items) ->
    io:fwrite("Found ~p files\n", [length(Items)]),
    lists:foreach(migrate_item_access(_), Items).

migrate_item_access(#{key := Key}) ->
    R = do([error_m ||
            {Server, FileID} <- extract_file_info(Key),
            check_for_local_metadata(Server, FileID),
            {Owner, Access} <- get_metadata(Server, FileID),
            wocky_db_tros:set_metadata(Server, FileID, Owner, Access)
           ]),
    case R of
        ok ->
            io:fwrite(".");
        {error, already_migrated} ->
            io:fwrite("-");
        {error, metadata_not_found} ->
            io:fwrite("N");
        {error, not_tros_file} ->
            io:fwrite("\nSkipping non-tros file: ~p", [Key]);
        {error, E} ->
            io:fwrite("\nError ~p on ~p\n", [E, Key])
    end.

extract_file_info(Key) ->
    case re:split(Key, "(.*)-[0-9a-f]{4}/(.*)") of
        [_, Server, FileID, _] -> {ok, {Server, FileID}};
        _ -> {error, not_tros_file}
    end.

check_for_local_metadata(Server, FileID) ->
    case wocky_db_tros:get_owner(Server, FileID) of
        not_found -> ok;
        _ -> {error, already_migrated}
    end.

get_metadata(Server, FileID) ->
    do([error_m ||
        Metadata <- mod_wocky_tros_s3_legacy:get_metadata(Server, FileID),
        Owner <- mod_wocky_tros_s3_legacy:get_owner(Metadata),
        Access <- mod_wocky_tros_s3_legacy:get_access(Metadata),
        {ok, {Owner, Access}}]).

%%%===================================================================
%%% Command implementation - fix_bot_images
%%%===================================================================

fix_bot_images() ->
    Q = "SELECT * FROM bot",
    Result = wocky_db:query(shared, Q, #{}, one),
    fix_bot_images(Result).

fix_bot_images(no_more_results) -> ok;
fix_bot_images({ok, Result}) ->
    lists:foreach(fix_images_on_bot(_), wocky_db:rows(Result)),
    fix_bot_images(wocky_db:fetch_more(Result)).

fix_images_on_bot(Bot = #{id          := ID,
                          description := Description,
                          server      := Server,
                          image       := BotImage}) ->
    io:fwrite("Bot: ~p - ~s\n", [binary_to_list(ID), Description]),
    Images = wocky_db_bot:item_images(Server, ID),
    ImageURLs = [I || #{image := I} <- Images],
    ValidImages = [I || I <- [BotImage | ImageURLs], I =/= <<>>],
    fix_images(Bot, ValidImages).

fix_images(Bot, Images) ->
    io:fwrite("~B images found...\n", [length(Images)]),
    Fixed = lists:foldl(maybe_fix_image(Bot, _, _), 0, Images),
    io:fwrite("~B needed fixing.\n", [Fixed]).

maybe_fix_image(#{id := BotID, server := BotServer}, Image, Acc) ->
    BotJID = wocky_bot_util:make_jid(BotServer, BotID),
    R = do([error_m ||
            {Server, FileID} <- tros:parse_url(Image),
            Metadata         <- tros:get_metadata(Server, FileID),
            Access           <- tros:get_access(Metadata),
            check_access(BotJID, Access),
            fix_access(BotJID, Server, FileID)
           ]),
    case R of
        ok ->
            io:fwrite("Fixed: ~p\n", [Image]),
            Acc + 1;
        {error, _} ->
            Acc
    end.

check_access(BotJID, Access) ->
    Rules = tros_permissions:access_rules_from_list(Access),
    case lists:any(is_bot_redirect(BotJID, _), Rules) of
        true -> {error, no_change_needed};
        false -> ok
    end.

is_bot_redirect(BotJID, {redirect, JID}) ->
    jid:are_equal(BotJID, JID);
is_bot_redirect(_BotJID, _) ->
    false.

fix_access(BotJID, Server, FileID) ->
    NewAccess = <<"redirect:", (jid:to_binary(BotJID))/binary>>,
    mod_wocky_tros_s3:update_access(Server, FileID, NewAccess).


%%%===================================================================
%%% Command implementation - make_token
%%%===================================================================

make_token(Handle) ->
    do([error_m ||
        #{user := User, server := Server} <- get_user(Handle),
        Resource <- make_resource(),
        Token <- get_token(User, Server, Resource),
        io:fwrite("User:     ~s\nResource: ~s\nToken:    ~s\n",
                  [User, Resource, Token])
       ]).

make_resource() ->
    Suffix = base16:encode(crypto:strong_rand_bytes(4)),
    {ok, <<"cli-resource-", Suffix/binary>>}.

get_token(User, Server, Resource) ->
    {ok, Token, _Expiry} = wocky_db_user:assign_token( User, Server, Resource),
    {ok, Token}.

%%%===================================================================
%%% Command implementation - reprocess_images
%%%===================================================================


reprocess_images() ->
    fun_chain:first(
      mod_wocky_tros_s3:bucket(),
      ?s3:list_objects(),
      ?ex_aws:'stream!'(s3_auth()),
      reprocess_images()
     ),
    ok.

reprocess_images(Stream) ->
    case ?enum:'empty?'(Stream) of
        true ->
            ok;
        false ->
            Head = ?enum:take(Stream, 3),
            Used = reprocess_image(Head),
            reprocess_images(?enum:drop(Stream, Used))
    end.

reprocess_image(Files = [ImageFile, MaybeOriginal, MaybeThumbnail]) ->
    BaseID = s3_key(ImageFile),
    OrigAndThumb = {tros:get_base_id(s3_key(MaybeOriginal)),
                    tros:get_base_id(s3_key(MaybeThumbnail))},
    Types = [tros:get_type(s3_key(F)) || F <- Files],
    case {OrigAndThumb, Types} of
        {{BaseID, BaseID}, [full, original, thumbnail]} ->
            do_reprocess_image(s3_key(MaybeOriginal)),
            3; % Use the original image, skip over the
               % full and thumbnail entries
        _ ->
            do_reprocess_image(BaseID),
            1
    end;

reprocess_image([ImageFile | _]) ->
    do_reprocess_image(s3_key(ImageFile)),
    1.

do_reprocess_image(ImageName) ->
    case tros:get_type(ImageName) of
        thumbnail ->
            io:fwrite("ERROR: Refusing to reprocess "
                      "orphaned thumbnail ~p\n", [ImageName]);
        _ ->
            % Copy the image back to the quarantine bucket to allow
            % the Lambda function to reprocess it.
            BaseID = tros:get_base_id(ImageName),
            Req = ?s3:put_object_copy(
                     <<(mod_wocky_tros_s3:bucket())/binary, "-quarantine">>,
                     BaseID,
                     mod_wocky_tros_s3:bucket(),
                     ImageName),
            ?ex_aws:'request!'(Req, s3_auth()),
            io:fwrite("Reprocessing ~p from ~p\n", [BaseID, ImageName])
    end.

%%%===================================================================
%%% Common helpers
%%%===================================================================

get_user(Handle) ->
    case wocky_db_user:find_user_by(handle, Handle) of
        not_found ->
            {error, "User '" ++ binary_to_list(Handle) ++ "' not found"};
        User ->
            {ok, User}
    end.

s3_auth() ->
    [{access_key_id, mod_wocky_tros_s3:access_key_id()},
     {secret_access_key, mod_wocky_tros_s3:secret_key()}].

s3_key(#{key := Key}) -> Key.
