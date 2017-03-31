use Mix.Config

config :honeybadger,
  environment_name: "HONEYBADGER_ENV"

import_config "prod.secret.exs"
