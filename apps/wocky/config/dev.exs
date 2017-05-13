use Mix.Config

config :wocky, Wocky.Repo,
  database: "wocky_dev"

config :honeybadger,
  environment_name: :dev
