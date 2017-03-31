use Mix.Config

# Configure your database
config :wocky, Wocky.Repo,
  adapter: Ecto.Adapters.MySQL,
  username: "root",
  password: "",
  database: "wocky_dev",
  hostname: "localhost",
  pool_size: 10

config :honeybadger,
  environment_name: "Development"
