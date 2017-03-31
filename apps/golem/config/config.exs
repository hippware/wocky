# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

config :golem,
  ecto_repos: [Golem.Repo]

import_config "#{Mix.env}.exs"
