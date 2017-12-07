use Mix.Config

config :wocky_xmpp,
  wocky_env: 'prod'

config :mnesia,
  dir: '${HOME}/var/mnesia/${HOSTNAME}'

config :honeybadger,
  environment_name: "${HONEYBADGER_ENV}"
