%%%----------------------------------------------------------------------
%%% File    : wocky_bot.hrl
%%% Author  : Bernard Duggan
%%% Purpose : Bot module header
%%%
%%%
%%% Copyright (C) 2016 Hippware
%%%----------------------------------------------------------------------

-ifndef(WOCKY_BOT_HRL).
-define(WOCKY_BOT_HRL, true).

-define(WOCKY_BOT_VIS_OWNER,        0).
-define(WOCKY_BOT_VIS_WHITELIST,    10).
-define(WOCKY_BOT_VIS_FRIENDS,      20).
-define(WOCKY_BOT_VIS_FOLLOWERS,    40).
-define(WOCKY_BOT_VIS_PUBLIC,       100).

-define(WOCKY_BOT_ALERT_DISABLED,   0).
-define(WOCKY_BOT_ALERT_ENABLED,    1).

-type bot_visibility() :: ?WOCKY_BOT_VIS_OWNER |
                          ?WOCKY_BOT_VIS_WHITELIST |
                          ?WOCKY_BOT_VIS_FRIENDS |
                          ?WOCKY_BOT_VIS_FOLLOWERS |
                          ?WOCKY_BOT_VIS_PUBLIC.

-type bot_alert_state() :: ?WOCKY_BOT_ALERT_DISABLED |
                           ?WOCKY_BOT_ALERT_ENABLED.

-endif. % WOCKY_BOT_HRL
