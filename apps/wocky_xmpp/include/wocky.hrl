%%%----------------------------------------------------------------------
%%% File    : wocky.hrl
%%% Author  : Beng Tan
%%% Purpose : A header stub
%%%
%%%
%%% Copyright (C) 2015 Hippware
%%%----------------------------------------------------------------------

-ifndef(WOCKY_HRL).
-define(WOCKY_HRL, true).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").

-define(HOME_STREAM_NODE, <<"home_stream">>).

%% Standard namespaces
-define(NS_ATOM,          <<"http://www.w3.org/2005/Atom">>).

%% Escalus defines NS_GEOLOC which causes problems when running tests
-ifndef(NS_GEOLOC).
-define(NS_GEOLOC,        <<"http://jabber.org/protocol/geoloc">>).
-endif.

%% Wocky namespaces
-define(NS_ACCESS,        <<"hippware.com/hxep/access">>).
-define(NS_BOT,           <<"hippware.com/hxep/bot">>).
-define(NS_BOT_EVENT,     <<"hippware.com/hxep/bot#event">>).
-define(NS_CONVERSATIONS_LEGACY, <<"hippware.com/hexp/conversations">>).
-define(NS_CONVERSATIONS, <<"hippware.com/hxep/conversations">>).
-define(NS_CRASH_TEST,    <<"hippware.com/hxep/crash-test">>).
-define(NS_ERRORS,        <<"hippware.com/hxep/errors">>).
-define(NS_HANDLE,        <<"hippware.com/hxep/handle">>).
-define(NS_NOTIFICATIONS, <<"hippware.com/hxep/notifications">>).
-define(NS_PHONE,         <<"hippware.com/hxep/phone">>).
-define(NS_PUBLISHING,    <<"hippware.com/hxep/publishing">>).
-define(NS_TOKEN,         <<"hippware.com/hxep/token">>).
-define(NS_TROS,          <<"hippware.com/hxep/http-file">>).
-define(NS_USER,          <<"hippware.com/hxep/user">>).

%% Elixir module names
-define(wocky_bot, 'Elixir.Wocky.Bot').
-define(wocky_item, 'Elixir.Wocky.Bot.Item').
-define(wocky_share, 'Elixir.Wocky.Bot.Share').
-define(wocky_subscription, 'Elixir.Wocky.Bot.Subscription').
-define(wocky_temp_subscription, 'Elixir.Wocky.Bot.TempSubscription').
-define(wocky_conversation, 'Elixir.Wocky.Conversation').
-define(wocky_factory, 'Elixir.Wocky.Repo.Factory').
-define(wocky_home_stream_item, 'Elixir.Wocky.HomeStreamItem').
-define(wocky_id, 'Elixir.Wocky.Repo.ID').
-define(wocky_errors, 'Elixir.Wocky.Repo.Errors').
-define(wocky_repo, 'Elixir.Wocky.Repo').
-define(wocky_roster_item, 'Elixir.Wocky.RosterItem').
-define(wocky_rsm_helper, 'Elixir.Wocky.RSMHelper').
-define(wocky_timestamp, 'Elixir.Wocky.Repo.Timestamp').
-define(wocky_token, 'Elixir.Wocky.Token').
-define(wocky_traffic_log, 'Elixir.Wocky.TrafficLog').
-define(wocky_user, 'Elixir.Wocky.User').

-define(tros, 'Elixir.Wocky.TROS').
-define(tros_s3, 'Elixir.Wocky.TROS.S3Store').
-define(tros_metadata, 'Elixir.Wocky.TROS.Metadata').

-define(wocky_event_handler, 'Elixir.Wocky.EventHandler').
-define(home_stream_handler, 'Elixir.Wocky.EventHandler.HomeStream').
-define(push_notification_handler,
         'Elixir.Wocky.EventHandler.PushNotification').
-define(new_message_event, 'Elixir.Wocky.Events.NewMessageEvent').
-define(bot_share_event, 'Elixir.Wocky.Events.BotShareEvent').

-define(wocky_push_notifier, 'Elixir.Wocky.PushNotifier').

-define(confex, 'Elixir.Confex').
-define(datetime, 'Elixir.DateTime').
-define(duration, 'Elixir.Timex.Duration').
-define(timex, 'Elixir.Timex').

-endif. % ifdef WOCKY_HRL
