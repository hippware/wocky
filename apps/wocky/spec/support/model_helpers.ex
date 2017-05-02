defmodule ModelHelpers do
  defmacro __using__(_args) do
    quote do
      import Ecto
      import Ecto.Changeset, except: [change: 1, change: 2]
      import Ecto.Query
      import ChangesetAssertions

      alias Wocky.Repo
      alias Wocky.Repo.Factory
    end
  end
end
