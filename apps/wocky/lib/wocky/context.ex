defmodule Wocky.Context do
  @moduledoc """
  Sets up a module to act as a context.

  This imports the Ecto goodness necessary to implement a context. It also
  helps define which modules are contexts via `use Wocky.Context` and ensures
  that contexts are behaving in a somewhat uniform way by only importing what
  is strictly necessary.
  """

  defmacro __using__(_) do
    quote do
      import Ecto, only: [assoc: 2, build_assoc: 2, build_assoc: 3]
      import Ecto.Query, only: [from: 2]

      alias Ecto.Queryable
      alias Wocky.Repo
    end
  end
end
