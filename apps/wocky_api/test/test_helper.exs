ExUnit.start()

Ecto.Adapters.SQL.Sandbox.mode(Wocky.Repo, :manual)
Absinthe.Test.prime(WockyAPI.Schema)
