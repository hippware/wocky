-module(wocky_db_bot).

-compile({parse_transform, cut}).

-export([get_bot/1,
         get_bot/2,
         owned_bots/1,
         subscribed_bots/1,
         get_id_by_name/2,
         exists/2,
         insert/2,
         insert_new_name/2,
         owner/2,
         visibility/2,
         affiliations/2,
         affiliations_from_map/1,
         update_affiliations/3,
         subscribers/2,
         delete/2,
         has_access/3,
         subscribe/3,
         unsubscribe/3,
         publish_item/5,
         get_item/3,
         get_items/2,
         delete_item/3,
         dissociate_user/2,
         image_items_count/2,
         item_images/2,
         set_follow_me/2,
         set_unfollow_me/1,
         subscribe_temporary/4,
         unsubscribe_temporary/3,
         clear_temporary_subscriptions/1
        ]).

% We're going to need these sooner or later, but for now stop xref complaining
-ignore_xref([followers/2, get_id_by_name/2]).

-include("wocky.hrl").
-include("wocky_bot.hrl").
-include("wocky_roster.hrl").
-include_lib("ejabberd/include/jlib.hrl").

-type shortname()           :: binary().
-type affiliation_type()    :: none | spectator | owner.
-type affiliate()           :: jid().
-type affiliation()         :: {affiliate(), affiliation_type()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec get_bot(jid()) -> map() | not_found.
get_bot(BotJID = #jid{lserver = Server}) ->
    get_bot(Server, wocky_bot_util:get_id_from_jid(BotJID)).

-spec get_bot(ejabberd:lserver(), wocky_db:id()) -> map() | not_found.
get_bot(_Server, ID) when is_binary(ID) ->
    wocky_db:select_row(shared, bot, all, #{id => ID}).

-spec owned_bots(jid()) -> [jid()].
owned_bots(UserJID) ->
    User = jid:to_binary(jid:to_bare(UserJID)),
    Bots = wocky_db:select(shared, user_bot, [id, server], #{owner => User}),
    [wocky_bot_util:make_jid(Server, ID) ||
     #{server := Server, id := ID} <- Bots].

-spec subscribed_bots(jid()) -> [jid()].
subscribed_bots(UserJID) ->
    User = jid:to_binary(jid:to_bare(UserJID)),
    Result = wocky_db:select(shared, subscribed_bot,
                             [bot, server], #{user => User}),

    Device = jid:to_binary(UserJID),
    TempResult = wocky_db:select(shared, temp_subscription,
                                 [bot, server], #{device => Device}),

    Bots = [wocky_bot_util:make_jid(Server, Bot)
            || #{bot := Bot, server := Server} <- Result ++ TempResult] ++
           owned_bots(UserJID),
    lists:usort(Bots).

-spec get_id_by_name(wocky_db:server(), shortname()) ->
    wocky_db:id() | not_found.
get_id_by_name(Server, Name) ->
    wocky_db:select_one(Server, bot_name, id, #{shortname => Name}).

-spec exists(wocky_db:server(), wocky_db:id()) -> boolean().
exists(_Server, ID) ->
    case wocky_db:select_one(shared, bot, id, #{id => ID}) of
        not_found -> false;
        _ -> true
    end.

-spec insert(wocky_db:server(), map()) -> ok.
insert(_Server, Fields) ->
    wocky_db:insert(shared, bot, Fields).

-spec insert_new_name(wocky_db:id(), shortname()) -> ok | {error, exists}.
insert_new_name(ID, Name) ->
    Vals = #{shortname => Name, id => ID},
    case wocky_db:insert_new(wocky_app:server(), bot_name, Vals) of
        true -> ok;
        false -> {error, exists}
    end.

-spec owner(wocky_db:server(), wocky_db:id()) -> jid() | not_found.
owner(_Server, ID) ->
    maybe_to_jid(wocky_db:select_one(shared, bot, owner, #{id => ID})).

-spec visibility(wocky_db:server(), wocky_db:id()) ->
    bot_visibility() | not_found.
visibility(_Server, ID) ->
    wocky_db:select_one(shared, bot, visibility, #{id => ID}).

-spec affiliations(wocky_db:server(), wocky_db:id()) ->
    [affiliation()].
affiliations(_Server, ID) ->
    Map = wocky_db:select_row(shared, bot, [affiliates, owner], #{id => ID}),
    affiliations_from_map(Map).

-spec affiliations_from_map(map() | not_found) -> [affiliation()] | not_found.
affiliations_from_map(not_found) -> not_found;
affiliations_from_map(Map = #{affiliates := null}) ->
    affiliations_from_map(Map#{affiliates => []});
affiliations_from_map(#{owner := Owner, affiliates := Affiliations}) ->
    owner_affiliation(Owner) ++
     [{jid:from_binary(A), spectator} || A <- Affiliations].

owner_affiliation(<<>>) -> [];
owner_affiliation(Owner) ->
    [{jid:from_binary(Owner), owner}].

-spec update_affiliations(wocky_db:server(), wocky_db:id(), [affiliation()]) ->
    ok.
update_affiliations(_Server, ID, Affiliations) ->
    {Add, Remove} = lists:partition(fun({_, Type}) -> Type =:= spectator end,
                                    Affiliations),
    AddQ = "UPDATE bot SET affiliates = affiliates + ? "
           "WHERE id = ? IF EXISTS",
    AddV = #{affiliates => user_parts(Add), id => ID},
    RemoveQ = "UPDATE bot SET affiliates = affiliates - ? "
              "WHERE id = ? IF EXISTS",
    RemoveV = #{affiliates => user_parts(Remove), id => ID},

    Add =/= [] andalso
        ({ok, _} = wocky_db:query(shared, AddQ, AddV, quorum)),
    Remove =/= [] andalso
        ({ok, _} = wocky_db:query(shared, RemoveQ, RemoveV, quorum)),
    ok.

-spec subscribers(wocky_db:server(), wocky_db:id()) -> [jid()].
subscribers(Server, ID) ->
    Result = wocky_db:select_column(shared, bot_subscriber,
                                    user, #{bot => ID}),
    TempResult = wocky_db:select_column(shared, bot_temp_subscription,
                                        device, #{bot => ID}),
    Subscribers = [jid:from_binary(J) || J <- Result ++ TempResult],
    AllSubscribers = maybe_add_owner_as_subscriber(owner(Server, ID),
                                                   Subscribers),
    wocky_util:remove_redundant_jids(AllSubscribers).

-spec delete(wocky_db:server(), wocky_db:id()) -> ok.
delete(Server, ID) ->
    ShortName = wocky_db:select_one(shared, bot, shortname, #{id => ID}),
    ok = delete_bot_name_lookup(Server, ShortName),
    ok = wocky_db:delete(shared, bot, all, #{id => ID}).

delete_bot_name_lookup(Server, Name)
  when is_binary(Name) andalso size(Name) > 0 ->
    wocky_db:delete(Server, bot_name, all, #{shortname => Name});
delete_bot_name_lookup(_, _) ->
    ok.

-spec has_access(wocky_db:server(), wocky_db:id(), jid()) ->
    boolean() | not_found.
has_access(_Server, ID, User) ->
    BareUserBin = jid:to_binary(jid:to_bare(User)),
    Bot =
    wocky_db:select_row(shared, bot,
                        [visibility, affiliates, owner],
                        #{id => ID}),
    has_access(BareUserBin, Bot).

-spec subscribe(wocky_db:server(), wocky_db:id(), jid()) -> ok.
subscribe(Server, ID, User) ->
    case owner(Server, ID) of
        User ->
            ok;
        _ ->
            ok = wocky_db:insert(
                   shared, bot_subscriber,
                   #{bot => ID,
                     server => Server,
                     user => jid:to_binary(jid:to_bare(User))})
    end.

-spec unsubscribe(wocky_db:server(), wocky_db:id(), jid()) -> ok.
unsubscribe(Server, ID, User) ->
    case owner(Server, ID) of
        User ->
            ok;
        _ ->
            ok = wocky_db:delete(shared, bot_subscriber, all,
                                 #{bot => ID,
                                   user => jid:to_binary(jid:to_bare(User))})
    end.

-spec publish_item(wocky_db:server(), wocky_db:id(), binary(),
                   binary(), boolean()) -> ok.
publish_item(Server, BotID, NoteID, Stanza, Image) ->
    Existing = wocky_db:select_one(Server, bot_item, id,
                                   #{id => NoteID, bot => BotID}),
    {NewItem, MaybePublished} = case Existing of
                                    not_found -> {true, #{published => now}};
                                    _ -> {false, #{}}
                                end,
    Note = MaybePublished#{id => NoteID,
                           bot => BotID,
                           updated => now,
                           stanza => Stanza,
                           image => Image
                          },
    wocky_db:insert(Server, bot_item, Note),
    maybe_bump_updated(Server, BotID, NewItem).

maybe_bump_updated(_Server, BotID, true) ->
    ok = wocky_db:insert(shared, bot, #{id => BotID, updated => now});
maybe_bump_updated(_, _, false) ->
    ok.

get_item(Server, BotID, NoteID) ->
    wocky_db:select_row(Server, bot_item, all, #{id => NoteID, bot => BotID}).

get_items(Server, BotID) ->
    wocky_db:select(Server, bot_item, all, #{bot => BotID}).

-spec delete_item(wocky_db:server(), wocky_db:id(), binary()) -> ok.
delete_item(Server, BotID, NoteID) ->
    wocky_db:delete(Server, bot_item, all, #{id => NoteID, bot => BotID}).

-spec dissociate_user(ejabberd:luser(), wocky_db:server()) -> ok.
dissociate_user(LUser, LServer) ->
    OwnedBots = owned_bots(jid:make(LUser, LServer, <<>>)),
    Roster = wocky_db_roster:get_roster(LUser, LServer),
    FriendJIDs = jid_list(Roster, wocky_db_roster:is_friend(_)),
    FollowerJIDs = jid_list(Roster, wocky_db_roster:is_follower(_)),
    lists:foreach(
      remove_owner(_, FriendJIDs, FollowerJIDs), OwnedBots).

jid_list(Roster, FilterFun) ->
    Filtered = lists:filter(FilterFun(_), Roster),
    [jid:make(J) || #wocky_roster{contact_jid = J} <- Filtered].

-spec image_items_count(wocky_db:server(), wocky_db:id()) -> non_neg_integer().
image_items_count(Server, BotID) ->
    wocky_db:count(Server, bot_item_images, #{bot => BotID, image => true}).

-spec item_images(wocky_db:server(), wocky_db:id()) ->
    [map()].
item_images(Server, BotID) ->
    R = wocky_db:select(Server, bot_item, [id, updated, stanza, image],
                        #{bot => BotID}),
    extract_images(R).

-spec set_follow_me(wocky_db:id(), integer()) -> ok.
set_follow_me(BotID, Expiry) ->
    ExpiryTS = wocky_db:seconds_to_timestamp(Expiry),
    wocky_db:update(shared, bot,
                    #{follow_me => true,
                      follow_me_expiry => ExpiryTS},
                    #{id => BotID}).

-spec set_unfollow_me(wocky_db:id()) -> ok.
set_unfollow_me(BotID) ->
    wocky_db:update(shared, bot,
                    #{follow_me => false,
                      follow_me_expiry => undefined},
                    #{id => BotID}).

-spec subscribe_temporary(ejabberd:lserver(), wocky_db:id(),
                          ejabberd:jid(), node()) -> ok.
subscribe_temporary(LServer, BotID, Device, Node) ->
    ok = wocky_db:insert(shared, temp_subscription,
                         #{device => jid:to_binary(Device),
                           bot    => BotID,
                           server => LServer,
                           node   => atom_to_binary(Node, utf8)}).

-spec unsubscribe_temporary(ejabberd:lserver(), wocky_db:id(),
                            ejabberd:jid()) -> ok.
unsubscribe_temporary(_LServer, BotID, Device) ->
    ok = wocky_db:delete(shared, temp_subscription, all,
                         #{device => jid:to_binary(Device),
                           bot    => BotID}).

-spec clear_temporary_subscriptions(node() | ejabberd:jid()) -> ok.
clear_temporary_subscriptions(Device = #jid{}) ->
    ok = wocky_db:delete(shared, temp_subscription, all,
                         #{device => jid:to_binary(Device)});

clear_temporary_subscriptions(Node) when is_atom(Node) ->
    Subscriptions = wocky_db:select(shared, node_temp_subscription,
                                    [device, bot],
                                    #{node => atom_to_binary(Node, utf8)}),
    lists:foreach(clear_temporary_subscription(_), Subscriptions).

clear_temporary_subscription(#{device := Device, bot := Bot}) ->
    ok = wocky_db:delete(shared, temp_subscription, all,
                         #{device => Device,
                           bot => Bot}).

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
                   owner:= Owner}) ->
    wocky_db_roster:is_friend(jid:from_binary(Owner),
                              jid:from_binary(User));
has_access(User, #{visibility := ?WOCKY_BOT_VIS_FOLLOWERS,
                   owner:= Owner}) ->
    wocky_db_roster:is_follower(jid:from_binary(Owner),
                                jid:from_binary(User));
has_access(_User, #{visibility := ?WOCKY_BOT_VIS_PUBLIC}) ->
    true;
has_access(_, _) ->
    false.

maybe_to_jid(not_found) ->
    not_found;
maybe_to_jid(null) ->
    [];
maybe_to_jid(<<>>) ->
    not_found;
maybe_to_jid(JIDBin) ->
    jid:from_binary(JIDBin).

remove_owner(BotJID, FriendJIDs, FollowerJIDs) ->
    Bot = get_bot(BotJID),
    NewBot = maybe_freeze_roster(Bot, FriendJIDs, FollowerJIDs),
    wocky_db:insert(shared, bot, NewBot#{owner => <<>>}).

maybe_freeze_roster(Bot = #{visibility := ?WOCKY_BOT_VIS_FRIENDS},
                    FriendJIDs, _FollowerJIDs) ->
    Bot#{visibility => ?WOCKY_BOT_VIS_WHITELIST,
         affiliates => [jid:to_binary(J) || J <- FriendJIDs]};
maybe_freeze_roster(Bot = #{visibility := ?WOCKY_BOT_VIS_FOLLOWERS},
                    _FriendJIDs, FollowerJIDs) ->
    Bot#{visibility => ?WOCKY_BOT_VIS_WHITELIST,
         affiliates => [jid:to_binary(J) || J <- FollowerJIDs]};
maybe_freeze_roster(Bot, _, _) -> Bot.

maybe_add_owner_as_subscriber(not_found, Subs) -> Subs;
maybe_add_owner_as_subscriber(Owner, Subs) -> [Owner | Subs].

extract_images(Items) ->
    lists:foldl(extract_image(_, _), [], Items).

extract_image(#{image := true, id := ID, updated := Updated, stanza := S},
              Acc) ->
    case wocky_bot_util:get_image(S) of
        none -> Acc;
        I -> [#{id => ID, updated => Updated, image => I} | Acc]
    end;
extract_image(_, Acc) -> Acc.
