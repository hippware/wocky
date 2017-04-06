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
-include("wocky.hrl").

-define(s3, 'Elixir.ExAws.S3').
-define(ex_aws, 'Elixir.ExAws').
-define(enum, 'Elixir.Enum').

%% gen_mod handlers
-export([start/2, stop/1]).

%% commands
-export([befriend/2,
         fix_bot_images/0,
         make_token/1,
         reprocess_images/0,
         reindex/1
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

     %% Regenerate Algolia indices
     #ejabberd_commands{name     = reindex,
                        desc     = "Regenerate Algolia indices",
                        longdesc = "Parameter: index to regenerate. eg: "
                                   "\"bots\" or \"users\"\n",
                        module   = ?MODULE,
                        function = reindex,
                        args     = [{index, binary}],
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

make_friend({#{id := User1, server := Server1},
             #{id := User2, server := Server2}}) ->
    JID2 = jid:to_binary(jid:make(User2, Server2, <<>>)),
    RosterItem = wocky_db_roster:get_roster_item(User1, Server1, JID2),
    RosterItem2 = RosterItem#wocky_roster{subscription = both,
                                          ask = none},
    wocky_db_roster:update_roster_item(User1, Server1, JID2, RosterItem2),
    ejabberd_hooks:run(roster_modified, wocky_xmpp_app:server(),
                       [User1, Server1, JID2]).

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
        #{id := User, server := Server} <- get_user(Handle),
        Resource <- make_resource(),
        Token <- get_token(User, Server, Resource),
        io:fwrite("User:     ~s\nResource: ~s\nToken:    ~s\n",
                  [User, Resource, Token])
       ]).

make_resource() ->
    Suffix = base16:encode(crypto:strong_rand_bytes(4)),
    {ok, <<"cli-resource-", Suffix/binary>>}.

get_token(User, Server, Resource) ->
    {ok, {Token, _Expiry}} = ?wocky_user_token:assign(User, Server, Resource),
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
%%% Reindex
%%%===================================================================

reindex(Index) ->
    try binary_to_existing_atom(Index, utf8) of
        IdxAtom ->
            case 'Elixir.Wocky.Index':reindex(IdxAtom) of
                {error, bad_call} -> io:fwrite("Unknown index\n");
                ok -> ok
            end
    catch
        error:badarg ->
            io:fwrite("Unknown index\n")
    end.

%%%===================================================================
%%% Common helpers
%%%===================================================================

get_user(Handle) ->
    case ?wocky_user:search(handle, Handle) of
        [] ->
            {error, "User '" ++ binary_to_list(Handle) ++ "' not found"};
        [User] ->
            {ok, User}
    end.

s3_auth() ->
    [{access_key_id, mod_wocky_tros_s3:access_key_id()},
     {secret_access_key, mod_wocky_tros_s3:secret_key()}].

s3_key(#{key := Key}) -> Key.
