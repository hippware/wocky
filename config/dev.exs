use Mix.Config

config :schemata,
  clusters: [
    [
      username: 'cassandra',
      password: 'cassandra',
      seed_hosts: ['127.0.0.1'],
      keyspaces: [
        wocky_shared: [
          strategy: :simple,
          factor: 1,
          clients: 1
        ],
        wocky_localhost: [
          strategy: :simple,
          factor: 1,
          clients: 1
        ],
        schemata: [
          strategy: :simple,
          factor: 1,
          clients: 1
        ]
      ]
    ]
  ]
