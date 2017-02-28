%%% @copyright 2017+ Hippware, Inc.
%%%
%%% @doc Module implementing Slack integration
%%%
-module(wocky_slack).

-export([post_weekly_bot_report/1]).

-ignore_xref([post_weekly_bot_report/1]).

-define(slack_files, 'Elixir.Slackex.Files').

-spec post_weekly_bot_report(binary()) -> map().
post_weekly_bot_report(Channel) ->
    Token = wocky_app:get_config(slack_token),
    Report = wocky_report:generate_bot_report(7),
    Server = wocky_app:server(),
    ?slack_files:upload(nil, #{content  => iolist_to_binary(Report),
                               token    => list_to_binary(Token),
                               filename => <<"weekly_bot_report",
                                             Server/binary, ".csv">>,
                               title    => <<"Weekly Bot Report for ",
                                             Server/binary>>,
                               filetype => <<"csv">>,
                               channels => Channel}).
