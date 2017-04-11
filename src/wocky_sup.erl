%%% @copyright 2015+ Hippware, Inc.
%%% @doc Top level wocky supervisor
-module(wocky_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(notification_broker, 'Elixir.Wocky.PushNotificationBroker').
-define(notification_handler, 'Elixir.Wocky.PushNotificationHandler').

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(CfgTerms) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, CfgTerms).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(CfgTerms) ->
    SupFlags = #{strategy  => one_for_one,
                 intensity => 1,
                 period    => 5},

    UserIdx = #{id       => wocky_index,
                start    => {'Elixir.Wocky.Index', start_link, []},
                restart  => permanent,
                shutdown => 5000,
                type     => worker,
                modules  => ['Elixir.Wocky.Index']},

    BotExpiryMon = #{id       => wocky_bot_expiry_mon,
                     start    => {wocky_bot_expiry_mon, start_link, []},
                     restart  => permanent,
                     shutdown => 5000,
                     type     => worker,
                     modules  => [wocky_bot_expiry_mon]},

    Cron = #{id       => wocky_cron,
             start    => {wocky_cron, start_link, []},
             restart  => permanent,
             shutdown => 5000,
             type     => supervisor
            },

    NotificationBroker = #{id => notification_broker,
                           start => {?notification_broker, start_link, []},
                           restart => permanent,
                           shutdown => 5000,
                           type => worker,
                           modules => [?notification_broker]},

    NotificationHandler = #{id => notification_handler,
                            start => {?notification_handler,
                                      start_link, [CfgTerms]},
                            restart => permanent,
                            shutdown => 5000,
                            type => worker,
                            modules => [?notification_handler]},


    {ok, {SupFlags, [UserIdx,
                     BotExpiryMon,
                     Cron,
                     NotificationBroker,
                     NotificationHandler
                    ]}}.
