Code.require_file("spec/support/assertions/be_valid_assertion.ex")
Code.require_file("spec/support/assertions/have_errors_assertion.ex")
Code.require_file("spec/support/changeset_assertions.ex")
Code.require_file("spec/support/test_event_handler.ex")
Code.require_file("spec/support/model_helpers.ex")
Code.require_file("spec/support/roster_helper.ex")

ESpec.configure fn config ->
  config.before fn tags ->
    :ok = case Ecto.Adapters.SQL.Sandbox.checkout(Wocky.Repo) do
            :ok -> :ok
            {:already, :owner} -> :ok
            error -> error
          end
    {:ok, tags: tags, server: "localhost"}
  end

  config.finally fn _shared ->
    Ecto.Adapters.SQL.Sandbox.checkin(Wocky.Repo, [])
  end
end

Ecto.Adapters.SQL.Sandbox.mode(Wocky.Repo, :manual)
