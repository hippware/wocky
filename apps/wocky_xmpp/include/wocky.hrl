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

-record(table_def, {
          name          :: atom(),
          columns       :: [{atom(), atom()
                                 | {set | list, atom()}
                                 | {map, atom(), atom()}}],
          primary_key   :: atom() | [[atom()] | atom()],
          order_by = [] :: atom() | [{atom(), asc | desc}]
         }).

-define(NS_ERRORS,        <<"hippware.com/hxep/errors">>).
-define(NS_TOKEN,         <<"hippware.com/hxep/token">>).
-define(NS_PHONE,         <<"hippware.com/hxep/phone">>).
-define(NS_HANDLE,        <<"hippware.com/hxep/handle">>).
-define(NS_BOT,           <<"hippware.com/hxep/bot">>).
-define(NS_BOT_EVENT,     <<"hippware.com/hxep/bot#event">>).
-define(NS_GROUP_CHAT,    <<"hippware.com/hxep/groupchat">>).
-define(NS_CONVERSATIONS, <<"hippware.com/hexp/conversations">>).
-define(NS_TROS,          <<"hippware.com/hxep/http-file">>).
-define(NS_WOCKY_ROSTER,  <<"hippware.com/hxep/roster">>).
-define(NS_USER,          <<"hippware.com/hxep/user">>).
-define(NS_ACCESS,        <<"hippware.com/hxep/access">>).
-define(NS_NOTIFICATIONS, <<"hippware.com/hxep/notifications">>).
-define(NS_CRASH_TEST,    <<"hippware.com/hxep/crash-test">>).
-define(NS_PUBLISHING,    <<"hippware.com/hxep/publishing">>).

%% Escalus defines NS_GEOLOC which causes problems when running tests
-ifndef(NS_GEOLOC).
-define(NS_GEOLOC,        <<"http://jabber.org/protocol/geoloc">>).
-endif.

-define(NS_ATOM,          <<"http://www.w3.org/2005/Atom">>).

-define(GROUP_CHAT_RESOURCE_PREFIX, "groupchat/").
-define(GROUP_CHAT_WITH_JID, <<"$$GROUP_CHAT$$">>).

-define(TOKEN_BYTES, 32).
-define(TOKEN_MARKER, "$T$").
-define(TOKEN_EXPIRE, 1209600). % two weeks in seconds

% Delay between sending result of a delete request and calling the
% delete hook (which terminates the connection). This is needed to
% ensure that the deleting user receives the IQ response before
% the connection is dropped.
-define(USER_DELETE_DELAY, 2000).

-define(HOME_STREAM_NODE, <<"home_stream">>).

% Elixir module names
-define(wocky_bot, 'Elixir.Wocky.Bot').
-define(wocky_item, 'Elixir.Wocky.Bot.Item').
-define(wocky_conversation, 'Elixir.Wocky.Conversation').
-define(wocky_factory, 'Elixir.Wocky.Repo.Factory').
-define(wocky_home_stream_item, 'Elixir.Wocky.HomeStreamItem').
-define(wocky_id, 'Elixir.Wocky.Repo.ID').
-define(wocky_errors, 'Elixir.Wocky.Repo.Errors').
-define(wocky_repo, 'Elixir.Wocky.Repo').
-define(wocky_roster_item, 'Elixir.Wocky.RosterItem').
-define(wocky_token, 'Elixir.Wocky.Token').
-define(wocky_timestamp, 'Elixir.Wocky.Repo.Timestamp').
-define(wocky_traffic_log, 'Elixir.Wocky.TrafficLog').
-define(wocky_user, 'Elixir.Wocky.User').

-define(tros, 'Elixir.Wocky.TROS').
-define(tros_s3, 'Elixir.Wocky.TROS.S3').
-define(tros_metadata, 'Elixir.Wocky.TROS.Metadata').

-define(wocky_event_handler, 'Elixir.Wocky.EventHandler').
-define(home_stream_handler, 'Elixir.Wocky.EventHandler.HomeStream').
-define(push_notification_handler,
         'Elixir.Wocky.EventHandler.PushNotification').
-define(new_message_event, 'Elixir.Wocky.Events.NewMessageEvent').
-define(bot_share_event, 'Elixir.Wocky.Events.BotShareEvent').

-define(wocky_push_notifier, 'Elixir.Wocky.PushNotifier').

-define(timex, 'Elixir.Timex').
-define(duration, 'Elixir.Timex.Duration').

% Standard time format to use unless there's a good reason to do otherwise
-define(DEFAULT_TIME_FORMAT, <<"{ISO:Extended}">>).


-endif. % ifdef WOCKY_HRL
