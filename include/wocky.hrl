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
-define(wocky_id, 'Elixir.Wocky.ID').
-define(wocky_repo, 'Elixir.Wocky.Repo').
-define(wocky_user, 'Elixir.Wocky.User').
-define(wocky_user_token, 'Elixir.Wocky.User.Token').


-endif. % ifdef WOCKY_HRL
