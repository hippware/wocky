use Mix.Config

config :wocky,
  wocky_env: 'test',
  keyspace_prefix: 'wocky_test_',
  config_dir: File.cwd |> elem(1) |> Path.join("etc") |> String.to_char_list

config :schemata,
  clusters: [
    default: [
      username: 'cassandra',
      password: 'cassandra',
      seed_hosts: ['127.0.0.1'],
      keyspaces: [
        wocky_test_shared: [
          strategy: :simple,
          factor: 1,
          clients: 10
        ],
        wocky_test_localhost: [
          strategy: :simple,
          factor: 1,
          clients: 10
        ]
      ]
    ]
  ]

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
