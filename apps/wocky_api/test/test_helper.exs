ExUnit.configure(formatters: [ExUnit.CLIFormatter, ExUnitNotifier])
ExUnit.start()

Ecto.Adapters.SQL.Sandbox.mode(Wocky.Repo, :manual)
Absinthe.Test.prime(WockyAPI.Schema)
