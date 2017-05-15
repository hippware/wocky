use Mix.Config

config :wocky, Wocky.Repo,
  database: "wocky_dev",
  pool_size: 10

config :honeybadger,
  environment_name: :dev
