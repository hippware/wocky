-module(wocky_db_bot).

-compile({parse_transform, cut}).

-export([get_bot/1,
         get_bot/2,
         owned_bots/1,
         subscribed_bots/1,
         get_id_by_name/2,
         exists/2,
         new_id/1,
         insert/2,
         insert_new_name/2,
         owner/2,
         is_public/2,
         subscribers/2,
         subscriber_count/2,
         delete/2,
         has_access/3,
         subscribe/3,
         unsubscribe/3,
         publish_item/5,
         get_item/3,
         get_items/2,
         delete_item/3,
         image_items_count/2,
         item_images/2,
         set_follow_me/2,
         set_unfollow_me/1,
         subscribe_temporary/4,
         unsubscribe_temporary/3,
         clear_temporary_subscriptions/1,
         add_share/3,
         is_shared_to/2,
         is_preallocated_id/2
        ]).

% We're going to need these sooner or later, but for now stop xref complaining
-ignore_xref([followers/2, get_id_by_name/2, is_shared_to/2]).

-include("wocky.hrl").
-include("wocky_bot.hrl").
-include_lib("ejabberd/include/jlib.hrl").

-define(NEW_ID_TTL, 3600 * 24). % IDs expire after one day if unused

-type shortname()           :: binary().

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

-spec new_id(ejabberd:jid()) -> wocky_db:id().
new_id(#jid{luser = LUser}) ->
    ID = ?wocky_id:new(),
    wocky_db:insert(shared, pending_bot, #{id => ID, owner => LUser,
                                           '[ttl]' => ?NEW_ID_TTL}),
    ID.

-spec insert(wocky_db:server(), map()) -> ok.
insert(_Server, Fields) ->
    ok = wocky_db:insert(shared, bot, Fields),
    'Elixir.Wocky.Index':bot_updated(maps:get(id, Fields), Fields).

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

-spec is_public(wocky_db:server(), wocky_db:id()) -> boolean() | not_found.
is_public(Server, ID) ->
    case visibility(Server, ID) of
        not_found -> not_found;
        ?WOCKY_BOT_VIS_OPEN -> true;
        _ -> false
    end.

-spec subscribers(wocky_db:server(), wocky_db:id()) -> [jid()].
subscribers(_Server, ID) ->
    Result = wocky_db:select_column(shared, bot_subscriber,
                                    user, #{bot => ID}),
    TempResult = wocky_db:select_column(shared, bot_temp_subscription,
                                        device, #{bot => ID}),
    Subscribers = [jid:from_binary(J) || J <- Result ++ TempResult],
    wocky_util:remove_redundant_jids(Subscribers).

-spec subscriber_count(wocky_db:server(), wocky_db:id()) -> non_neg_integer().
subscriber_count(Server, ID) ->
    Count = wocky_db:count(shared, bot_subscriber, #{bot => ID}),
    case owner(Server, ID) of
        not_found -> Count;
        _ -> Count + 1
    end.

-spec delete(wocky_db:server(), wocky_db:id()) -> ok.
delete(Server, ID) ->
    ShortName = wocky_db:select_one(shared, bot, shortname, #{id => ID}),
    ok = delete_bot_name_lookup(Server, ShortName),
    ok = wocky_db:delete(shared, bot, all, #{id => ID}),
    'Elixir.Wocky.Index':bot_removed(ID).

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
                        [id, server, visibility, owner],
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

-spec add_share(ejabberd:jid(), ejabberd:jid(), ejabberd:jid()) -> ok.
add_share(From, To, BotJID) ->
    ok = wocky_db:insert(shared, bot_share,
                         #{from_jid => jid:to_binary(jid:to_bare(From)),
                           to_jid   => jid:to_binary(jid:to_bare(To)),
                           bot      => jid:to_binary(BotJID),
                           time     => now}).

-spec is_shared_to(ejabberd:jid(), ejabberd:jid()) -> boolean().
is_shared_to(User, BotJID) ->
    not_found =/= wocky_db:select_one(
                    shared, bot_share, bot,
                    #{bot    => jid:to_binary(BotJID),
                      to_jid => jid:to_binary(jid:to_bare(User))}).

-spec is_preallocated_id(ejabberd:jid(), wocky_db:id()) -> boolean().
is_preallocated_id(#jid{luser = LUser}, ID) ->
    not_found =/= wocky_db:select_one(
                    shared, pending_bot, id,
                    #{owner => LUser, id => ID}).

%%%===================================================================
%%% Private helpers
%%%===================================================================

has_access(_, not_found) ->
    not_found;
has_access(_, #{visibility := ?WOCKY_BOT_VIS_OPEN}) ->
    true;
has_access(User, #{owner := User}) ->
    true;
has_access(User, #{id := ID, server := Server}) ->
    UserJID = jid:from_binary(User),
    is_shared_to(UserJID, wocky_bot_util:make_jid(Server, ID)).

maybe_to_jid(not_found) ->
    not_found;
maybe_to_jid(null) ->
    [];
maybe_to_jid(<<>>) ->
    not_found;
maybe_to_jid(JIDBin) ->
    jid:from_binary(JIDBin).

extract_images(Items) ->
    lists:foldl(extract_image(_, _), [], Items).

extract_image(#{image := true, id := ID, updated := Updated, stanza := S},
              Acc) ->
    case wocky_bot_util:get_image(S) of
        none -> Acc;
        I -> [#{id => ID, updated => Updated, image => I} | Acc]
    end;
extract_image(_, Acc) -> Acc.
