%%% @copyright 2017+ Hippware, Inc.
%%%
%%% @doc Module implementing wocky_publishing_handler for bots.
%%%
-module(wocky_bot_publishing).

-compile({parse_transform, do}).

-behaviour(wocky_publishing_handler).

-include("wocky.hrl").
-include("wocky_bot.hrl").

% wocky_publishing_handler exports
-export([publish/4,
         delete/2,
         get/3,
         subscribe/3,
         unsubscribe/2
        ]).

publish(_, _, _, _) ->
    {error,
     ?ERRT_NOT_ACCEPTABLE(?MYLANG, <<"Publishing to bots not available">>)}.

delete(_, _) ->
    {error,
     ?ERRT_NOT_ACCEPTABLE(
        ?MYLANG, <<"Deleteing via publishing interface not available">>)}.

get(_, _, _) ->
    {error,
     ?ERRT_NOT_ACCEPTABLE(
        ?MYLANG, <<"Getting via publishing interface not available">>)}.

subscribe(TargetJID, UserJID, _) ->
    do([error_m ||
        Bot <- wocky_bot_util:get_bot_from_jid(TargetJID),
        User <- {ok, ?wocky_user:get_by_jid(UserJID)},
        wocky_bot_util:check_access(User, Bot),
        wocky_watcher:watch(bot, UserJID, TargetJID)
       ]).

unsubscribe(TargetJID, UserJID) ->
    wocky_watcher:unwatch(bot, UserJID, TargetJID).
