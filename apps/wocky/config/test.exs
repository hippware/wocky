use Mix.Config

config :wocky,
  indexing_system: "test",
  firebase_load_on_startup: false,
  bot_report_channel: "report-testing"

config :wocky, Wocky.Account.Firebase,
  load_keys_on_startup: false,
  local_keys: [
    {
      "c947c408c8dd053f7e13117c4e00f0b2b16dc789",
      """
      -----BEGIN CERTIFICATE-----
      MIIDBjCCAe4CCQCgEOA/+QkMRTANBgkqhkiG9w0BAQsFADBFMQswCQYDVQQGEwJB
      VTETMBEGA1UECAwKU29tZS1TdGF0ZTEhMB8GA1UECgwYSW50ZXJuZXQgV2lkZ2l0
      cyBQdHkgTHRkMB4XDTE3MDgyMjA3NDE1OFoXDTQ1MDEwNjA3NDE1OFowRTELMAkG
      A1UEBhMCQVUxEzARBgNVBAgMClNvbWUtU3RhdGUxITAfBgNVBAoMGEludGVybmV0
      IFdpZGdpdHMgUHR5IEx0ZDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEB
      ALdtBHIjCHlhvDQF3WQ15aHnupKlQprzrus9LdTCp3jYsZEOYLRkk0EHeY3ontY1
      oA76TAnUekYuRDk61BwrMGYqmmvX3Obp8AU4Wzn18MwdjloExh8ZoR2qeka/v5uN
      sUMRy1LBeI7ND75uvzzvUpQ9c0Tj/weN6X4dhA7utJYe4yreFQGymt9sBQoWjPVV
      oPscsFnGDiIQD6mMZn3KR8R9C0hhlI0aM2Zk/9eYTMZgX58a4qCKCOJnpW93vTev
      fSwVuNSFpoiYg5diTdu3RFhjEHHQJE4j7HVpOwRx7NBLAqxV4Sn8Gl6YToyiaASm
      Hys0Xc1I/vdLda5A/SMJfd0CAwEAATANBgkqhkiG9w0BAQsFAAOCAQEAX1k2rWvP
      GJSiLoQuenA3iOT0BmRZYQSG/0uHMFaJOb42Xb3V0F35sjyYDGqH/yDI8/MlA0mq
      /BzkOG+5yQM2F3EjqAnFvO7eFfRHxU0E9xuiweFIx615sYTID6xvvbFFtZ1xD1YV
      EV3G88wSk1g6NYz2BfzpY089JrRNvLApNUk7ssemLOY/FMu+1bI6TNxgn1MU6zHK
      MlV3DJZAUdOZOyx77p4QQVH0BaPWSkNsmUXNQu/8aNbNktuiQN65+ByBPvq6W64u
      oVwftZpq3axVJyOZ9sfISPLUPvsHvK5r24kaSxFKknKO7X8Jb1FUGJ0UidgBi1pf
      hYWUqOAgiBY3RQ==
      -----END CERTIFICATE-----
      """
    }
  ]

config :wocky, Wocky.Push,
  enabled: true,
  sandbox: true,
  logging: false

# Configure your database
config :wocky, Wocky.Repo,
  database: "wocky_test",
  pool: Ecto.Adapters.SQL.Sandbox,
  # ms (30 minutes)
  ownership_timeout: 1_800_000

# Make token tests go faster:
config :bcrypt_elixir, bcrypt_log_rounds: 4

# Config for wocky_db_watcher to allow us to run it in tests to test the client
config :wocky_db_watcher, :db,
  database: {:system, :string, "WOCKY_DB_NAME", "wocky_test"},
  username: {:system, :string, "WOCKY_DB_USER", "postgres"},
  password: {:system, :string, "WOCKY_DB_PASSWORD", "password"},
  hostname: {:system, :string, "WOCKY_DB_HOST", "localhost"},
  port: {:system, :integer, "WOCKY_DB_PORT", 5432},
  pool_size: {:system, :integer, "WOCKY_DB_POOL_SIZE", 15},
  pool: Ecto.Adapters.SQL.Sandbox
