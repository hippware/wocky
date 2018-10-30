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

-export([
         maybe_notify_desc_change/2,
         notify_watchers/3
        ]).

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
            notify_watchers(
              NewBot, ?wocky_bot:to_jid(NewBot),
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
%%% FromJID is the jid from which the notification will be sent
%%%===================================================================

-spec notify_watchers(?wocky_bot:t(), ejabberd:jid(), jlib:xmlel()) -> ok.
notify_watchers(Bot, FromJID, Message) ->
    % Watchers
    Watchers = wocky_watcher:watchers(bot, ?wocky_bot:to_jid(Bot)),
    lists:foreach(notify_watcher(_, FromJID, Bot, Message), Watchers).

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
