# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

config :wocky,
  ecto_repos: [Wocky.Repo],
  tros_backend: Wocky.TROS.TestStore,
  tros_s3_bucket: "wocky-tros-test",
  enable_bot_event_notifications: true,
  enable_follow_me_updates: true,
  async_location_processing: false

config :wocky, Wocky.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: "postgres",
  password: "password",
  hostname: "localhost",
  pool_size: 10

config :logger,
  level: :info

config :email_checker,
  validations: [EmailChecker.Check.Format]

config :ex_aws,
  access_key_id: [
    {:system, "AWS_ACCESS_KEY_ID"},
    {:awscli, "default", 30},
    :instance_role
  ],
  secret_access_key: [
    {:system, "AWS_SECRET_ACCESS_KEY"},
    {:awscli, "default", 30},
    :instance_role
  ]

config :slackex,
  slack_token: "xoxb-141728662948-FN75kAhQfnpwil6HbAi5LIQg"

config :algolia,
  application_id: "HIE75ZR7Q7"

import_config "#{Mix.env}.exs"
