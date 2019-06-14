ExUnit.configure(formatters: [ExUnit.CLIFormatter, ExUnitNotifier])
ExUnit.start()

Ecto.Adapters.SQL.Sandbox.mode(Wocky.Repo, :manual)
Application.ensure_all_started(:bypass)
