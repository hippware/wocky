%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing Wocky bots - user management operations
%%% See https://github.com/hippware/tr-wiki/wiki/Bot
%%%
-module(wocky_bot_users).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-include("wocky.hrl").
-include("wocky_roster.hrl").

-export([handle_share/3,
         notify_new_viewers/4]).


%%%===================================================================
%%% Action - bot shared
%%%===================================================================

handle_share(_From, _To, error) -> drop;
handle_share(From, To, BotJID) ->
    Result = do([error_m ||
                 Bot <- wocky_bot_util:get_bot_from_jid(BotJID),
                 Sharer <- wocky_bot_util:get_user_from_jid(From),
                 check_can_share(Sharer, Bot),
                 Recipient <- wocky_bot_util:get_user_from_jid(To),
                 ?wocky_bot:share(Bot, Recipient, Sharer),
                 send_notification(From, To, BotJID)
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

send_notification(From, To, BotJID) ->
    Event = ?bot_share_event:new(#{from => From, to => To, bot => BotJID}),
    ?wocky_event_handler:broadcast(Event).

%%%===================================================================
%%% Access change notifications
%%%===================================================================

-spec notify_new_viewers(Server :: ejabberd:lserver(),
                         Bot :: ?wocky_bot:t(),
                         OldPublic :: boolean() | none,
                         NewPublic :: boolean()) -> ok.

% Unchanged visibility
notify_new_viewers(_, _, Vis, Vis) -> ok;

% New visibility is public - notify friends and followers
notify_new_viewers(Server, #{user_id := Owner} = Bot, _, true) ->
    notify_new_viewers(Server, Bot, get_friends_and_followers(Owner));

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
    [roster_to_jid(wocky_roster:to_wocky_roster(R))
     || R <- ?wocky_roster_item:followers(LUser)].

roster_to_jid(#wocky_roster{contact_jid = SimpleJID}) ->
    jid:make(SimpleJID).
