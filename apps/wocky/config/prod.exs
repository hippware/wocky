use Mix.Config

config :wocky,
  tros_s3_bucket: "wocky-tros-WOCKY_INST"
  tros_s3_access_key_id: "TROS_S3_ACCESS_KEY_ID"
  tros_s3_secret_key: "TROS_S3_SECRET_KEY"

config :honeybadger,
  environment_name: "HONEYBADGER_ENV"

import_config "prod.secret.exs"
