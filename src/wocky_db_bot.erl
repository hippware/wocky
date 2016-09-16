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
         subscribers/2,
         follow_state/3,
         followers/2,
         delete/2,
         has_access/3,
         subscribe/4,
         unsubscribe/3,
         owner_roster/2,
         owner_roster_ver/2,
         update_owner_roster/4
        ]).

% We're going to need these sooner or later, but for now stop xref complaining
-ignore_xref([followers/2, get_id_by_name/2]).

-include("wocky.hrl").
-include("wocky_bot.hrl").
-include_lib("ejabberd/include/jlib.hrl").

-type shortname()           :: binary().
-type affiliation_type()    :: none | spectator | owner.
-type affiliate()           :: jid().
-type affiliation()         :: {affiliate(), affiliation_type()}.

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

-spec owner(wocky_db:server(), wocky_db:id()) -> jid() | not_found.
owner(Server, ID) ->
    maybe_to_jid(wocky_db:select_one(Server, bot, owner, #{id => ID})).

-spec affiliations(wocky_db:server(), wocky_db:id()) ->
    [affiliation()].
affiliations(Server, ID) ->
    Map = wocky_db:select_row(Server, bot, [affiliates, owner], #{id => ID}),
    affiliations_from_map(Map).

-spec affiliations_from_map(map() | not_found) -> [affiliation()] | not_found.
affiliations_from_map(not_found) -> not_found;
affiliations_from_map(Map = #{affiliates := null}) ->
    affiliations_from_map(Map#{affiliates => []});
affiliations_from_map(#{owner := Owner, affiliates := Affiliations}) ->
    [owner_affiliation(Owner) |
     [{jid:from_binary(A), spectator} || A <- Affiliations]].

owner_affiliation(Owner) ->
    {jid:from_binary(Owner), owner}.

-spec update_affiliations(wocky_db:server(), wocky_db:id(), [affiliation()]) ->
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

-spec followers(wocky_db:server(), wocky_db:id()) -> [jid()].
followers(Server, ID) ->
    [J || {J, F} <- subscribers(Server, ID), F =:= true].

-spec subscribers(wocky_db:server(), wocky_db:id()) -> [{jid(), boolean()}].
subscribers(Server, ID) ->
    Result = wocky_db:select(Server, bot_subscriber,
                             [user, follow], #{bot => ID}),
    [{jid:from_binary(J), F} || #{user := J, follow := F} <- Result].

-spec follow_state(wocky_db:server(), wocky_db:id(), jid()) ->
    boolean() | not_found.
follow_state(Server, ID, User) ->
    UserBin = jid:to_binary(jid:to_bare(User)),
    wocky_db:select_one(Server, bot_subscriber, follow,
                        #{bot => ID, user => UserBin}).

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
                        [visibility, affiliates, owner, owner_roster],
                        #{id => ID}),
    has_access(BareUser, Bot).

-spec subscribe(wocky_db:server(), wocky_db:id(), jid(), boolean()) -> ok.
subscribe(Server, ID, User, Follow) ->
    ok = wocky_db:insert(
           Server, bot_subscriber,
           #{bot => ID,
             user => jid:to_binary(jid:to_bare(User)),
             follow => Follow}).

-spec unsubscribe(wocky_db:server(), wocky_db:id(), jid()) -> ok.
unsubscribe(Server, ID, User) ->
    ok = wocky_db:delete(Server, bot_subscriber, all,
                         #{bot => ID,
                           user => jid:to_binary(jid:to_bare(User))}).

-spec owner_roster(wocky_db:server(), wocky_db:id()) -> [jid()] | not_found.
owner_roster(Server, ID) ->
    maybe_to_jid(wocky_db:select_one(Server, bot, owner_roster, #{id => ID})).

-spec owner_roster_ver(wocky_db:server(), wocky_db:id()) ->
    binary() | not_found.
owner_roster_ver(Server, ID) ->
    wocky_db:select_one(Server, bot, owner_roster_ver, #{id => ID}).

-spec update_owner_roster(wocky_db:server(), wocky_db:id(),
                          [jid()], binary()) -> ok.
update_owner_roster(Server, ID, Items, Version) ->
    wocky_db:update(Server, bot,
                    #{owner_roster => [jid:to_binary(I) || I <-Items],
                      owner_roster_ver => Version},
                    #{id => ID}).

%%%===================================================================
%%% Private helpers
%%%===================================================================

user_parts(Affiliates) ->
    [jid:to_binary(User) || {User, _Role} <- Affiliates].

has_access(_, not_found) ->
    not_found;
has_access(User, #{owner := User}) ->
    true;
has_access(User, #{visibility := ?WOCKY_BOT_VIS_WHITELIST,
                   affiliates := Affiliates}) ->
    lists:member(User, wocky_util:null_to_list(Affiliates));
has_access(User, #{visibility := ?WOCKY_BOT_VIS_FRIENDS,
                   owner_roster := RosterMembers}) ->
    lists:member(User, wocky_util:null_to_list(RosterMembers));
has_access(_User, #{visibility := ?WOCKY_BOT_VIS_PUBLIC}) ->
    true;
has_access(_, _) ->
    false.

maybe_to_jid(not_found) ->
    not_found;
maybe_to_jid(null) ->
    [];
maybe_to_jid(JIDList) when is_list(JIDList) ->
    [jid:from_binary(J) || J <- JIDList];
maybe_to_jid(JIDBin) ->
    jid:from_binary(JIDBin).
