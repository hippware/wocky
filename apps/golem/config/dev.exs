use Mix.Config

# Configure your database
config :golem, Golem.Repo,
  adapter: Ecto.Adapters.MySQL,
  username: "root",
  password: "",
  database: "golem_dev",
  hostname: "localhost",
  pool_size: 10
