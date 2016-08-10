-module(wocky_db_bot).

-export([get/2,
         get_id_by_name/2,
         exists/2,
         insert/2,
         insert_new_name/2,
         owner/2,
         affiliations/2,
         affiliations_from_map/1,
         update_affiliations/3,
         followers/2,
         delete/2,
         has_access/3,
         follow/3,
         unfollow/3
        ]).

-include("wocky.hrl").
-include("wocky_bot.hrl").
-include_lib("ejabberd/include/jlib.hrl").

-type user()        :: binary().
-type shortname()   :: binary().
-type affiliation() :: none | spectator | owner.
-type affiliate()   :: {user(), affiliation()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec get(wocky_db:server(), wocky_db:id()) -> map() | not_found.
get(Server, ID) ->
    wocky_db:select_row(Server, bot, all, #{id => ID}).

-spec get_id_by_name(wocky_db:server(), shortname()) ->
    wocky_db:id() | not_found.
get_id_by_name(Server, Name) ->
    wocky_db:select_one(Server, bot_name, id, #{shortname => Name}).

-spec exists(wocky_db:server(), wocky_db:id()) -> boolean().
exists(Server, ID) ->
    case wocky_db:select_one(Server, bot, id, #{id => ID}) of
        not_found -> false;
        _ -> true
    end.

-spec insert(wocky_db:server(), map()) -> ok.
insert(Server, Fields) ->
    wocky_db:insert(Server, bot, Fields).

-spec insert_new_name(wocky_db:id(), shortname()) -> ok | {error, exists}.
insert_new_name(ID, Name) ->
    Vals = #{shortname => Name, id => ID},
    case wocky_db:insert_new(wocky_app:server(), bot_name, Vals) of
        true -> ok;
        false -> {error, exists}
    end.

-spec owner(wocky_db:server(), wocky_db:id()) -> user() | not_found.
owner(Server, ID) ->
    wocky_db:select_one(Server, bot, owner, #{id => ID}).

-spec affiliations(wocky_db:server(), wocky_db:id()) ->
    [affiliate()].
affiliations(Server, ID) ->
    Map = wocky_db:select_row(Server, bot, [affiliates, owner], #{id => ID}),
    affiliations_from_map(Map).

-spec affiliations_from_map(map() | not_found) -> [affiliate()] | not_found.
affiliations_from_map(not_found) -> not_found;
affiliations_from_map(#{owner := Owner, affiliates := Affiliations}) ->
    [{Owner, owner} | [{A, spectator} || A <- Affiliations]].

-spec update_affiliations(wocky_db:server(), wocky_db:id(), [affiliate()]) ->
    ok.
update_affiliations(Server, ID, Affiliations) ->
    {Add, Remove} = lists:partition(fun({_, Type}) -> Type =:= spectator end,
                                    Affiliations),
    AddQ = "UPDATE bot SET affiliates = affiliates + ? "
           "WHERE id = ? IF EXISTS",
    AddV = #{affiliates => user_parts(Add), id => ID},
    RemoveQ = "UPDATE bot SET affiliates = affiliates - ? "
              "WHERE id = ? IF EXISTS",
    RemoveV = #{affiliates => user_parts(Remove), id => ID},

    Add =/= [] andalso
        ({ok, _} = wocky_db:query(Server, AddQ, AddV, quorum)),
    Remove =/= [] andalso
        ({ok, _} = wocky_db:query(Server, RemoveQ, RemoveV, quorum)),
    ok.

-spec followers(wocky_db:server(), wocky_db:id()) -> [user()].
followers(Server, ID) ->
    wocky_db:select_one(Server, bot, followers, #{id => ID}).

-spec delete(wocky_db:server(), wocky_db:id()) -> ok.
delete(Server, ID) ->
    Shortname = wocky_db:select_one(Server, bot, shortname, #{id => ID}),
    Shortname =/= not_found andalso
    Shortname =/= null andalso
        (ok = wocky_db:delete(Server, bot_name, all,
                              #{shortname => Shortname})),
    ok = wocky_db:delete(Server, bot, all, #{id => ID}).

-spec has_access(wocky_db:server(), wocky_db:id(), jid()) ->
    boolean() | not_found.
has_access(Server, ID, User) ->
    BareUser = jid:to_binary(jid:to_bare(User)),
    Bot =
    wocky_db:select_row(Server, bot,
                        [visibility, owner, affiliates, followers],
                        #{id => ID}),
    has_access(BareUser, Bot).

-spec follow(wocky_db:server(), wocky_db:id(), jid()) -> ok.
follow(Server, ID, User) ->
    change_follow(Server, ID, User, $+).

-spec unfollow(wocky_db:server(), wocky_db:id(), jid()) -> ok.
unfollow(Server, ID, User) ->
    change_follow(Server, ID, User, $-).


%%%===================================================================
%%% Private helpers
%%%===================================================================

user_parts(Affiliates) ->
    [User || {User, _Role} <- Affiliates].

has_access(_, not_found) ->
    not_found;
has_access(User, #{owner := User}) ->
    true;
has_access(User, #{visibility := ?WOCKY_BOT_VIS_WHITELIST,
                   affiliates := Affiliates}) ->
    lists:member(User, Affiliates);
has_access(User, #{visibility := ?WOCKY_BOT_VIS_FRIENDS,
                   owner := Owner}) ->
    UserJID = jid:from_binary(User),
    OwnerJID = jid:from_binary(Owner),
    wocky_db_roster:has_contact(OwnerJID, UserJID);
has_access(_User, #{visibility := ?WOCKY_BOT_VIS_PUBLIC}) ->
    true;
has_access(_, _) ->
    false.

change_follow(Server, ID, User, Op) ->
    UserBin = jid:to_binary(jid:to_bare(User)),
    Q = ["UPDATE bot SET followers = followers ", Op, " ? WHERE id = ?"],
    V = #{id => ID,
          followers => [UserBin]
         },
    {ok, _} = wocky_db:query(Server, Q, V, quorum),
    ok.
