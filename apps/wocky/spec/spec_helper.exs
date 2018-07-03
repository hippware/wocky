ESpec.configure(fn config ->
  ESpec.Configuration.before(
    fn tags ->
      :ok =
        case Ecto.Adapters.SQL.Sandbox.checkout(Wocky.Repo) do
          :ok -> :ok
          {:already, :owner} -> :ok
          error -> error
        end

      {:ok, tags: tags}
    end,
    config
  )

  ESpec.Configuration.finally(
    fn _shared ->
      Ecto.Adapters.SQL.Sandbox.checkin(Wocky.Repo, [])
    end,
    config
  )
end)

Ecto.Adapters.SQL.Sandbox.mode(Wocky.Repo, :manual)
