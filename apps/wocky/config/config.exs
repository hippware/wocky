# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

config :wocky,
  ecto_repos: [Wocky.Repo]

config :email_checker,
  validations: [EmailChecker.Check.Format]

config :algolia,
  application_id: "HIE75ZR7Q7",
  api_key: "ALGOLIA_KEY"

# config :logger,
#   handle_otp_reports: false,
#   backends: [loggerlagerbackend],
#   level: :debug

config :honeybadger,
  exclude_envs: [:dev, :test],
  api_key: "HONEYBADGER_KEY"

import_config "#{Mix.env}.exs"
