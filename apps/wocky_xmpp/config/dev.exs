use Mix.Config

config :wocky_xmpp,
  wocky_env: 'dev'

config :schemata,
  cluster: [
    username: 'cassandra',
    password: 'cassandra',
    seed_hosts: [
      {'127.0.0.1', 9042}
    ],
    keyspaces: [
      wocky_shared: [
        strategy: :simple,
        factor: 1,
        clients: 10
      ],
      wocky_localhost: [
        strategy: :simple,
        factor: 1,
        clients: 10
      ]
    ]
  ]

config :lager,
  extra_sinks: [
    error_logger_lager_event: [
      handlers: [
        lager_file_backend: [file: 'error_logger.log', level: :info]
      ]
    ]
  ]

# We don't actually want this to do anything, but having it here verifies that
# crone will start up correctly
config :crone,
  tasks: [
    {
       {:weekly, :sun, {12, :am}},
       {:wocky_slack, :post_bot_report, ["report-testing", 7]}
    }
   ]
