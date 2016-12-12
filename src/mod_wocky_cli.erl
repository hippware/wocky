%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing Wocky CLI commands
%%%
-module(mod_wocky_cli).

-behaviour(gen_mod).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-include_lib("ejabberd/include/ejabberd_commands.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include("wocky_roster.hrl").

%% gen_mod handlers
-export([start/2, stop/1]).

%% commands
-export([befriend/2,
         tros_migrate/0,
         tros_cleanup/0]).

-ignore_xref([befriend/2,
              tros_migrate/0,
              tros_cleanup/0]).

%%%===================================================================
%%% gen_mod handlers
%%%===================================================================

start(_Host, _Opts) ->
    ejabberd_commands:register_commands(commands()).

stop(_Host) ->
    ejabberd_commands:unregister_commands(commands()).

commands() ->
    [#ejabberd_commands{name     = befriend,
                        desc     = "Make two users friends",
                        module   = ?MODULE,
                        function = befriend,
                        args     = [{user1, binary}, {user2, binary}],
                        result   = {result, restuple}},

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

     #ejabberd_commands{name     = dump_traffic,
                        desc     = "Dump traffic for a specified user",
                        module   = traffic_dumper,
                        function = dump,
                        args     = [{user, binary},
                                    {start, binary},
                                    {duration, binary}],
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

get_user(Handle) ->
    case wocky_db_user:find_user_by(handle, Handle) of
        not_found ->
            {error, "User '" ++ binary_to_list(Handle) ++ "' not found"};
        User ->
            {ok, User}
    end.

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
