use Mix.Config

config :wocky, Wocky.Repo,
  database: "wocky_dev",
  pool_size: 10

config :wocky, Wocky.Mailer,
  adapter: {:system, :module, "BAMBOO_ADAPTER", Bamboo.TestAdapter},
  api_key: {:system, :string, "MANDRILL_API_KEY"}
