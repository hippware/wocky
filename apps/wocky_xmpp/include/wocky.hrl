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

-include_lib("mongooseim/include/jlib.hrl").
-include_lib("mongooseim/include/ejabberd.hrl").

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
-define(wocky_account,           'Elixir.Wocky.Account').
-define(wocky_blocking,          'Elixir.Wocky.Blocking').
-define(wocky_bot,               'Elixir.Wocky.Bot').
-define(wocky_geosearch,         'Elixir.Wocky.Bot.Geosearch').
-define(wocky_item,              'Elixir.Wocky.Bot.Item').
-define(wocky_share,             'Elixir.Wocky.Bot.Share').
-define(wocky_conversation,      'Elixir.Wocky.Conversation').
-define(wocky_geo_utils,         'Elixir.Wocky.GeoUtils').
-define(wocky_home_stream,       'Elixir.Wocky.HomeStream').
-define(wocky_home_stream_id,    'Elixir.Wocky.HomeStream.ID').
-define(wocky_home_stream_item,  'Elixir.Wocky.HomeStream.Item').
-define(wocky_index,             'Elixir.Wocky.Index').
-define(wocky_new_follower_event, 'Elixir.Wocky.Push.Events.NewFollowerEvent').
-define(wocky_repo,              'Elixir.Wocky.Repo').
-define(wocky_errors,            'Elixir.Wocky.Repo.Errors').
-define(wocky_factory,           'Elixir.Wocky.Repo.Factory').
-define(wocky_id,                'Elixir.Wocky.Repo.ID').
-define(wocky_timestamp,         'Elixir.Wocky.Repo.Timestamp').
-define(wocky_roster,            'Elixir.Wocky.Roster').
-define(wocky_roster_item,       'Elixir.Wocky.Roster.Item').
-define(wocky_rsm_helper,        'Elixir.Wocky.RSMHelper').
-define(wocky_traffic_log,       'Elixir.Wocky.TrafficLog').
-define(wocky_user,              'Elixir.Wocky.User').
-define(wocky_watcher_client,    'Elixir.Wocky.Watcher.Client').

-define(wocky_xmpp_bot_callbacks, 'Elixir.WockyXMPP.BotCallbacks').
-define(wocky_xmpp_bot_share_callbacks,
        'Elixir.WockyXMPP.BotShareCallbacks').
-define(wocky_xmpp_home_stream_item_callbacks,
        'Elixir.WockyXMPP.HomeStreamItemCallbacks').
-define(wocky_xmpp_tros_metadata_callbacks,
        'Elixir.WockyXMPP.TROSMetadataCallbacks').

-define(tros, 'Elixir.Wocky.TROS').
-define(tros_s3, 'Elixir.Wocky.TROS.S3Store').
-define(tros_metadata, 'Elixir.Wocky.TROS.Metadata').

-define(wocky_push, 'Elixir.Wocky.Push').
-define(new_message_event, 'Elixir.Wocky.Push.Events.NewMessageEvent').
-define(new_follower_event, 'Elixir.Wocky.Push.Events.NewFollowerEvent').
-define(bot_share_event, 'Elixir.Wocky.Push.Events.BotShareEvent').

-define(confex, 'Elixir.Confex').
-define(datetime, 'Elixir.DateTime').
-define(duration, 'Elixir.Timex.Duration').
-define(timex, 'Elixir.Timex').

-endif. % ifdef WOCKY_HRL
