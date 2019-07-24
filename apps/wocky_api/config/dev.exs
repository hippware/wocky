use Mix.Config

config :wocky_api,
  allow_factory_insert: true,
  enable_location_request_trigger: true

# For development, we disable any cache and enable
# debugging and code reloading.
#
# The watchers configuration can be used to run external
# watchers to your application. For example, we use it
# with brunch.io to recompile .js and .css sources.
config :wocky_api, WockyAPI.Endpoint,
  debug_errors: true,
  code_reloader: true,
  check_origin: false,
  watchers: []
