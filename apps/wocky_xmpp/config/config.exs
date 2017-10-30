# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

config :wocky_xmpp,
  ecto_repos: [],
  hs_prepopulation_user: "__new_user_hs_archive__"

config :ejabberd,
  keep_lager_intact: true

config :kernel,
  start_pg2: :true

config :ssl,
  session_lifetime: 600 # 10 minutes

config :mnesia,
  dir: 'log/#{Mix.env}/mnesia'

config :setup,
  verify_directories: false

config :alarms,
  large_heap: 10_000_000

import_config "#{Mix.env}.exs"
