use Mix.Config

config :wocky,
  config_dir: String.to_char_list(Path.join(elem(File.cwd, 1), "etc"))

config :kernel,
  error_logger: :silent

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
