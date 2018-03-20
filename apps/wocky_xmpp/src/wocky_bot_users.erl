%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing Wocky bots - user management operations
%%% See https://github.com/hippware/tr-wiki/wiki/Bot
%%%
-module(wocky_bot_users).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-include("wocky.hrl").
-include("wocky_publishing.hrl").
-include("wocky_roster.hrl").

-export([handle_share/3,
         notify_new_viewers/4,
         maybe_notify_desc_change/2,
         notify_subscribers_and_watchers/4,
         send_share_notification/3,
         send_geofence_share_notification/3
        ]).


%%%===================================================================
%%% Action - bot shared
%%%===================================================================

handle_share(_From, _To, none) -> drop;
handle_share(From, To, Bot) ->
    Result = do([error_m ||
                 Sharer <- wocky_bot_util:get_user_from_jid(From),
                 check_can_share(Sharer, Bot),
                 Recipient <- wocky_bot_util:get_user_from_jid(To),
                 ?wocky_share:put(Recipient, Bot, Sharer)
                ]),
    case Result of
        ok -> ok;
        _ -> drop
    end.

check_can_share(Sharer, Bot) ->
    case ?wocky_bot:'public?'(Bot) of
        true ->
            ok;
        false ->
            case ?wocky_user:'owns?'(Sharer, Bot) of
                true -> ok;
                false -> {error, cant_share}
            end
    end.

send_share_notification(From, To = #{id := UserID}, Bot) ->
    Event = ?bot_share_event:new(#{from => From, to => To, bot => Bot}),
    ?wocky_push:notify_all(UserID, Event).

send_geofence_share_notification(From, To = #{id := UserID}, Bot)
    when From =/= nil ->
    Event = ?bot_geofence_share_event:new(
               #{from => From, to => To, bot => Bot}),
    ?wocky_push:notify_all(UserID, Event);
send_geofence_share_notification(_, _, _) -> ok.


%%%===================================================================
%%% Access change notifications
%%%===================================================================

-spec notify_new_viewers(Server :: ejabberd:lserver(),
                         Bot :: ?wocky_bot:t(),
                         OldPublic :: boolean() | none,
                         NewPublic :: boolean()) -> ok.

% Unchanged visibility
notify_new_viewers(_, _, Vis, Vis) -> ok;

% Newly created public bot or new visibility is public.
% Notify friends, followers and the creator
notify_new_viewers(Server, #{user_id := Owner} = Bot, _, true) ->
    notify_new_viewers(Server, Bot,
                       [jid:make(Owner, Server, <<>>)
                        | get_friends_and_followers(Owner)]);

% Any other case does not generate notification
notify_new_viewers(_, _, _, _) -> ok.

notify_new_viewers(Server, Bot, NewViewers) ->
    lists:foreach(notify_new_viewer(Server, Bot, _), NewViewers).

notify_new_viewer(Server, Bot, Viewer) ->
    ejabberd_router:route(jid:make(<<>>, Server, <<>>),
                          Viewer, bot_visible_stanza(Bot)).

bot_visible_stanza(Bot) ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"type">>, <<"headline">>}],
           children = [#xmlel{name = <<"bot">>,
                              attrs = [{<<"xmlns">>, ?NS_BOT}],
                              children = bot_visible_children(Bot)}]}.

bot_visible_children(#{id := ID} = Bot) ->
    [cdata_el(<<"jid">>, jid:to_binary(?wocky_bot:to_jid(Bot))),
     cdata_el(<<"id">>, ID),
     cdata_el(<<"server">>, wocky_xmpp_app:server()),
     cdata_el(<<"action">>, <<"show">>)].

cdata_el(Name, Value) ->
    #xmlel{name = Name,
           children = [#xmlcdata{content = Value}]}.

get_friends_and_followers(LUser) ->
    [?wocky_user:to_jid(R) || R <- ?wocky_roster:followers(LUser)].

%%%===================================================================
%%% Notify bot subscribers of changes to the bot description
%%%
%%% Currently this only encompases (non-whitespace) changes to the
%%% description
%%%===================================================================

-spec maybe_notify_desc_change(OldBot :: ?wocky_bot:t(),
                               NewBot :: ?wocky_bot:t()) -> ok.
maybe_notify_desc_change(#{description := OldDesc},
                         NewBot = #{description := NewDesc}) ->
    Old = wocky_util:remove_whitespace(OldDesc),
    New = wocky_util:remove_whitespace(NewDesc),
    case New of
        Old -> ok; % No change
        <<>> -> ok; % New version is only whitespace
        _ ->
            Owner = ?wocky_bot:owner(NewBot),
            notify_subscribers_and_watchers(
              NewBot, Owner, ?wocky_bot:to_jid(NewBot),
              desc_change_stanza(NewBot, Old, Owner))
    end.

desc_change_stanza(NewBot, OldDesc, User) ->
    {ok, BotEl} = mod_wocky_bot:make_bot_el(NewBot, User),
    #xmlel{name = <<"bot-description-changed">>,
           attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = maybe_new_tag(OldDesc) ++ [BotEl]}.

%%%===================================================================
%%% Send notification to bot subscribers
%%%
%%% Actor is the user who performed the action that triggered the
%%% notification. They will be excluded from the set of notify targets
%%%
%%% FromJID is the jid from which the notification will be sent
%%%===================================================================

-spec notify_subscribers_and_watchers(?wocky_bot:t(), ?wocky_user:t(),
                                      ejabberd:jid(), jlib:xmlel()) -> ok.
notify_subscribers_and_watchers(Bot, Actor, FromJID, Message) ->
    % Subscribers
    Recipients = ?wocky_bot:notification_recipients(Bot, Actor),
    lists:foreach(notify_subscriber(_, FromJID, Message), Recipients),

    % Watchers
    Watchers = wocky_watcher:watchers(bot, ?wocky_bot:to_jid(Bot)),
    lists:foreach(notify_watcher(_, FromJID, Bot, Message), Watchers).

notify_subscriber(UserJID, FromJID, Message) ->
    Stanza = #xmlel{name = <<"message">>,
                    attrs = [{<<"type">>, <<"headline">>}],
                    children = [Message]},
    ejabberd_router:route(FromJID, UserJID, Stanza).

notify_watcher(UserJID, FromJID, Bot = #{updated_at := UpatedAt}, Message) ->
    BotJID = ?wocky_bot:to_jid(Bot),
    Stanza = #xmlel{name = <<"message">>,
                    attrs = [{<<"type">>, <<"headline">>}],
                    children = [Message]},

    Timestamp = ?wocky_timestamp:to_string(UpatedAt),
    Item = #published_item{
              id = jid:to_binary(BotJID),
              version = Timestamp,
              from = FromJID,
              stanza = Stanza},
    wocky_publishing_handler:send_notification(UserJID, BotJID, Item).

maybe_new_tag(<<>>) ->  [#xmlel{name = <<"new">>}];
maybe_new_tag(_) -> [].
