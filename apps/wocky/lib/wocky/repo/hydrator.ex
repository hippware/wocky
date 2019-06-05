defmodule Wocky.Repo.Hydrator do
  @moduledoc """
  Helper for loading fields from the DB and checking that the load was
  successful.
  """

  alias Wocky.Repo

  @spec with_assocs(struct(), [atom()], (struct() -> any())) :: any()
  def with_assocs(record!, assocs, fun) do
    record! = Repo.preload(record!, assocs)

    if Enum.all?(assocs, fn a -> Map.get(record!, a) != nil end) do
      fun.(record!)
    else
      :hydration_failed
    end
  end
end
