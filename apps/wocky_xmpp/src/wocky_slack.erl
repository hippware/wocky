%%% @copyright 2017+ Hippware, Inc.
%%%
%%% @doc Module implementing Slack integration
%%%
-module(wocky_slack).

-export([post_bot_report/2]).

-define(wocky_bot_report, 'Elixir.Wocky.BotReport').


-spec post_bot_report(binary(), non_neg_integer()) -> binary().
post_bot_report(Channel, Days) ->
    ?wocky_bot_report:post_bot_report(wocky_xmpp_app:server(), Channel, Days).
