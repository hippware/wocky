use Mix.Config

config :wocky,
  wocky_env: 'test',
  location_api_port: 3000,
  enable_bot_event_notifications: true,
  enable_follow_me_updates: true,
  config_dir: File.cwd |> elem(1) |> Path.join("etc") |> String.to_char_list

config :logger,
  level: :error

config :lager,
  handlers: [
    {:lager_console_backend, [
      :critical,
      {:lager_default_formatter, [:time, :color, ' [', :severity, '] ', :message, '\e[0m\r\n']}
    ]},
    {:lager_file_backend, [
      {:file, 'wocky.log'},
      {:level, :warning},
      {:size, 2097152},
      {:date, '$D0'},
      {:count, 5}
    ]},
    {:lager_file_backend, [
      {:file, 'debug.log'},
      {:level, :debug},
      {:size, 2097152},
      {:date, '$D0'},
      {:count, 5}
    ]}
  ]

config :honeybadger,
  environment_name: "Testing"
