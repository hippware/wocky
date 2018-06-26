use Mix.Config

config :wocky,
  indexing_system: "test",
  firebase_load_on_startup: false,
  bot_report_channel: "report-testing"

config :guardian_firebase,
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
      """,
      """
      -----BEGIN PRIVATE KEY-----
      MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQC3bQRyIwh5Ybw0
      Bd1kNeWh57qSpUKa867rPS3Uwqd42LGRDmC0ZJNBB3mN6J7WNaAO+kwJ1HpGLkQ5
      OtQcKzBmKppr19zm6fAFOFs59fDMHY5aBMYfGaEdqnpGv7+bjbFDEctSwXiOzQ++
      br8871KUPXNE4/8Hjel+HYQO7rSWHuMq3hUBsprfbAUKFoz1VaD7HLBZxg4iEA+p
      jGZ9ykfEfQtIYZSNGjNmZP/XmEzGYF+fGuKgigjiZ6Vvd703r30sFbjUhaaImIOX
      Yk3bt0RYYxBx0CROI+x1aTsEcezQSwKsVeEp/BpemE6MomgEph8rNF3NSP73S3Wu
      QP0jCX3dAgMBAAECggEAad+fJVJbXdSwEUchVupVNXLQGj0RiOcHG/kgLyJ8ECDj
      vVqTLwyugmaSHvsaU4J4dKy8nx/pxACImJAARpIXSaFlqMHcW9zEEf9JiNcQuoCE
      3ijLQsBEYx83nQaozlym6JOozIen0qVCZST/dWiePbqKgkUnu3CKSaU3yHa3/b2z
      eM1tZ/0aCxxTiRn/E0snBBGWMs3XuFcp/nLgw3Nbd1XG6skgOegxqfmCtdNyywzl
      PksfbLepk73axPUbTkkz8WFUOnfrTED4cGo++3VrfaMBLjM1Sm4jiQr9FIB3NmfF
      j4b/Ui3ZNoG1xbjK0xDsuKsl6tLGzNVaFk23PUwkAQKBgQDpyjkrQLlU0FLxg8AS
      k/0lwV7JjMbUQadDr49kQDGOkz/ljlZgStv8jQFsGYpvNIKZi5OXWxNPB3mWQwU3
      z7dF7e47gXCMzvmIrdr1Gs2YPxTYjv9wB3om2D6mLyBs/4yCaGo8hi0SPVYZi2iC
      uQLv3g2UTOKaX9rCmQ0jDQetHQKBgQDI2fA04+drvSQvIFrqk4Skz2g8sN7+7j/n
      yx/aYMJyCNCpvih5L2014xXosTVbNhn5pmFUlZ+4qwwQEzElQgqN2VZdHNxE0bYe
      xTyjSuKkJY0jAcfUJz0neiXbWpKvfj3yQupFNeWjpOE2tsstFfWkH89xL6w/1VIs
      CI/u6M33wQKBgEyHevFSrZg63Xvboesy8GIEi4+0en2OxD8e3/R0IwTF5NuzHUlG
      F/7y9W06axt99+ZlTznzgT2Ud9OdOr8LSrYkbaCi/YHKWtrH9m3XiUd2Fs/Q94Ln
      n6/Jh7CEqrujZ45kuan4ThazZ1TTUrG/+FsmuBE8nczk5cpfqXI42LNtAoGBAJ1/
      gMQIvs0WWUx3I7P0j7wpRATrcUIZE5WxC75Tx8ZiMTYZ/mThEtOByglZBI0MxJum
      o4YPelr2DhSA6DXeLqaC+h0z52ozxIsmgWFO9KBhLeZ4m/k599OADjWPNZ1V8j+J
      x2kUVYnYXh5ogrRNFv1nUGTiTEEWB0SuRifC+NhBAoGAcXiz/l+vO1xa0+qpWFO/
      ZomXwqs+Do40vXqWQg7C7fJ3C3Wgf1RO5MRncVpcawqPkN9jvskFtA18B4lXN6f+
      MRcUPLMy+tbWNleZ/KktLk5GdGeY8GaScFb82EwaWb2NIs1MF/ONyGF5sTZvToK7
      hHku1vi9fpU/eNt0FXgecd8=
      -----END PRIVATE KEY-----
      """
    }
  ]

config :wocky, Wocky.Push,
  enabled: true,
  sandbox: true,
  reflect: true,
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
