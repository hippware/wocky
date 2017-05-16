use Mix.Config

config :wocky_xmpp,
  wocky_env: 'test',
  config_dir: File.cwd |> elem(1) |> Path.join("etc") |> String.to_char_list

config :honeybadger,
  environment_name: :test

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
