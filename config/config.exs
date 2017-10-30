# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

# By default, the umbrella project as well as each child
# application will require this configuration file, ensuring
# they all use the same configuration. While one could
# configure all applications here, we prefer to delegate
# back to each application for organization purposes.
import_config "../apps/*/config/config.exs"

# Configure Logging

# Let Logger handle error_logger logs
config :sasl, :sasl_error_logger, false

config :logger,
  truncate: :infinity,
  backends: [:console],
  compile_time_purge_level: :info,
  level: :info

config :logger, :console,
  format: "$date $time [$level] $levelpad$metadata$message\n",
  # Include Ecto logging metadata
  metadata: [:query, :query_params, :queue_time, :query_time, :decode_time,
             :db_duration]

# Stop lager redirecting :error_logger messages
config :lager, :error_logger_redirect, false
# Stop lager removing Logger's :error_logger handler
config :lager, :error_logger_whitelist, [Logger.ErrorHandler]
# Stop lager writing a crash log
config :lager, :crash_log, false
# Use LagerLogger as lager's only handler.
config :lager, :handlers, [{LagerLogger, [level: :info]}]

# Exometer uses Hut as a logging abstraction
config :hut, :level, :info

# Configure release generation
config :distillery,
  no_warn_missing: [
    :distillery,
    :exactor,
    :dialyxir,
    :edown,
    :escalus,
    :exref,
    :fun_chain,
    :ok,
    :hamcrest,
    :meck,
    :mix_ct,
    :mix_elvis,
    :mustache,
    :parse_trans,
    :proper,
    :elixir_make
  ]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
