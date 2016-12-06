%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing Wocky CLI commands
%%%
-module(mod_wocky_cli).

-behaviour(gen_mod).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-include_lib("ejabberd/include/ejabberd_commands.hrl").
-include("wocky_roster.hrl").

%% gen_mod handlers
-export([start/2, stop/1]).

%% commands
-export([befriend/2]).

-ignore_xref([befriend/2]).

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
                        result   = {result, restuple}}
    ].

%%%===================================================================
%%% Command implementation
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
