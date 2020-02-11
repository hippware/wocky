[
  ~r|lib/common_graphql_client/client.ex:.*|,
  {":0:unknown_type Unknown type: Continuation.t/0."},
  {"lib/wocky/repo/instrumenter.ex", :unmatched_return, 3},
  {"lib/wocky_api/metrics/pipeline_instrumenter.ex", :unmatched_return, 4},
  {"lib/wocky_api/router.ex", :unmatched_return, 35},
  {"lib/wocky/notifier/in_band/notification.ex", :contract_supertype, 95}

  # Logger.info ignores:
  # These filters appear to be unnecessary in Elixir 1.10
  # {"lib/wocky/repo/cleaner.ex", :unmatched_return},
  # {"lib/wocky/application.ex", :unmatched_return},
  # {"lib/wocky/sms/sandbox.ex", :unmatched_return, 12}
]
