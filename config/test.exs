use Mix.Config

config :wocky,
  wocky_env: 'test',
  keyspace_prefix: 'wocky_test_',
  config_dir: File.cwd |> elem(1) |> Path.join("etc") |> String.to_char_list

config :cqerl,
  text_uuids: true,
  client_groups: [
    client_group: [
      name: :no_ks,
      hosts: ['localhost'],
      opts: [
        auth: {:cqerl_auth_plain_handler, [{'cassandra', 'cassandra'}]},
      ],
      clients_per_server: 1
    ],
    client_group: [
      name: :shared_ks,
      hosts: ['localhost'],
      opts: [
        keyspace: :wocky_test_shared,
        auth: {:cqerl_auth_plain_handler, [{'cassandra', 'cassandra'}]}
      ],
      clients_per_server: 10
    ],
    client_group: [
      name: :local_ks,
      hosts: ['localhost'],
      opts: [
        keyspace: :wocky_test_localhost,
        auth: {:cqerl_auth_plain_handler, [{'cassandra', 'cassandra'}]}
      ],
      clients_per_server: 10
    ]
  ]

config :kernel,
  error_logger: :silent

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
