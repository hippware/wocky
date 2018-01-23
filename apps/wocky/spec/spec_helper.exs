ESpec.configure(fn config ->
  config.before(fn tags ->
    :ok =
      case Ecto.Adapters.SQL.Sandbox.checkout(Wocky.Repo) do
        :ok -> :ok
        {:already, :owner} -> :ok
        error -> error
      end

    {:ok, tags: tags, server: "localhost"}
  end)

  config.finally(fn _shared ->
    Ecto.Adapters.SQL.Sandbox.checkin(Wocky.Repo, [])
  end)
end)

Ecto.Adapters.SQL.Sandbox.mode(Wocky.Repo, :manual)
