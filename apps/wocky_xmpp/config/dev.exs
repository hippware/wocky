use Mix.Config

config :wocky_xmpp,
  wocky_env: 'dev'

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
