import Config

alias Wocky.Config.VaultAdapter

config :wocky, :redis, password: {{:via, VaultAdapter}, "redis-password", nil}

config :wocky, :redlock,
  servers: [
    [
      host: {:system, :string, "REDIS_HOST", "localhost"},
      port: {:system, :integer, "REDIS_PORT", 6379},
      ssl: {:system, :boolean, "REDIS_SSL", false},
      auth: {{:via, VaultAdapter}, "redis-password", nil},
      database: {:system, :integer, "REDIS_DB", 0}
    ]
  ]

config :fun_with_flags, :redis,
  password: {{:via, VaultAdapter}, "redis-password"}
