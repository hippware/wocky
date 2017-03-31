use Mix.Config

# Configure your database
config :golem, Golem.Repo,
  adapter: Ecto.Adapters.MySQL,
  username: "root",
  password: "",
  database: "golem_test",
  hostname: "localhost",
  pool: Ecto.Adapters.SQL.Sandbox
