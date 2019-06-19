[
  ~r|apps/wocky/lib/wocky/account/jwt/.*\.ex:.:contract_supertype|,
  ~r|Function Wocky\..*\.__impl__/1 does not exist.|,
  {"apps/wocky/lib/wocky/repo/instrumenter.ex", :unmatched_return, 3},
  {"apps/wocky_api/lib/wocky_api/pipeline_instrumenter.ex", :unmatched_return, 3},
  {"apps/wocky_api/lib/wocky_api/router.ex", :unmatched_return, 35},
  {"apps/wocky_api/test/support/channel_helper.ex", :unmatched_return, 29},

  # Logger.info ignores:
  {"apps/wocky/lib/wocky/repo/cleaner.ex", :unmatched_return},
  {"apps/wocky/lib/wocky/application.ex", :unmatched_return},
  {"apps/wocky/lib/wocky/sms/sandbox.ex", :unmatched_return, 11},
  {"apps/wocky/lib/wocky/tasks/loc_share_expire.ex", :unmatched_return, 15}
]
