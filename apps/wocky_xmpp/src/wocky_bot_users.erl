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
         notify_new_viewers/4,
         maybe_notify_subscribers/3]).


%%%===================================================================
%%% Action - bot shared
%%%===================================================================

handle_share(_From, _To, none) -> drop;
handle_share(From, To, Bot) ->
    Result = do([error_m ||
                 Sharer <- wocky_bot_util:get_user_from_jid(From),
                 check_can_share(Sharer, Bot),
                 Recipient <- wocky_bot_util:get_user_from_jid(To),
                 ?wocky_share:put(Recipient, Bot, Sharer),
                 send_notification(Sharer, Recipient, Bot)
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

send_notification(From, To, Bot) ->
    Event = ?bot_share_event:new(#{from => From, to => To, bot => Bot}),
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
    [?wocky_user:to_jid(R) || R <- ?wocky_roster_item:followers(LUser)].

%%%===================================================================
%%% Notify non-temp bot subscribers of changes to the bot
%%%
%%% Currently this only encompases (non-whitespace) changes to the
%%% description
%%%===================================================================

-spec maybe_notify_subscribers(Server :: ejabberd:lserver(),
                               OldBot :: ?wocky_bot:t(),
                               NewBot :: ?wocky_bot:t()) -> ok.
maybe_notify_subscribers(Server,
                         #{description := OldDesc},
                         NewBot = #{description := NewDesc}) ->
    Old = wocky_util:remove_whitespace(OldDesc),
    New = wocky_util:remove_whitespace(NewDesc),
    case New of
        Old -> ok; % No change
        <<>> -> ok; % New version is only whitespace
        _ -> notify_subscribers(NewBot, Old, Server)
    end.

notify_subscribers(NewBot, OldDesc, Server) ->
    Subscribers = ?wocky_bot:non_temp_subscribers(NewBot),
    lists:foreach(notify_desc_change(NewBot, OldDesc, _, Server), Subscribers).

notify_desc_change(NewBot, OldDesc, User, Server) ->
    ejabberd_router:route(jid:make(<<>>, Server, <<>>),
                          ?wocky_user:to_jid(User),
                          desc_change_stanza(NewBot, OldDesc, User)).

desc_change_stanza(NewBot, OldDesc, User) ->
    {ok, BotEl} = mod_wocky_bot:make_bot_el(NewBot, User),
    #xmlel{name = <<"message">>,
           attrs = [{<<"type">>, <<"headline">>}],
           children = [#xmlel{name = <<"bot-description-changed">>,
                              attrs = [{<<"xmlns">>, ?NS_BOT}],
                              children = maybe_new_tag(OldDesc) ++ [BotEl]}]}.

maybe_new_tag(<<>>) ->  [#xmlel{name = <<"new">>}];
maybe_new_tag(_) -> [].
