%%% @copyright 2017+ Hippware, Inc.
%%%
%%% @doc Module implementing Slack integration
%%%
-module(wocky_slack).

-export([post_bot_report/2]).

-define(slack_files, 'Elixir.Slackex.Files').


-spec post_bot_report(binary(), non_neg_integer()) -> binary().
post_bot_report(Channel, Days) ->
    Token = wocky_xmpp_app:get_config(slack_token),
    Report = wocky_report:generate_bot_report(Days),
    Server = wocky_xmpp_app:server(),
    ?slack_files:upload(#{content  => iolist_to_binary(Report),
                          token    => list_to_binary(Token),
                          filename => <<"weekly_bot_report",
                                        Server/binary, ".csv">>,
                          title    => <<"Weekly Bot Report for ",
                                        Server/binary>>,
                          filetype => <<"csv">>,
                          channels => Channel}).
