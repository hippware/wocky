[
  ~r|lib/common_graphql_client/client.ex:.*|,
  {"lib/wocky/repo/instrumenter.ex", :unmatched_return, 3},
  {"lib/wocky_api/pipeline_instrumenter.ex", :unmatched_return, 3},
  {"lib/wocky_api/router.ex", :unmatched_return, 35},

  # Logger.info ignores:
  {"lib/wocky/repo/cleaner.ex", :unmatched_return},
  {"lib/wocky/application.ex", :unmatched_return},
  {"lib/wocky/sms/sandbox.ex", :unmatched_return, 12}
]
