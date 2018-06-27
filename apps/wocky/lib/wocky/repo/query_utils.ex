defmodule Wocky.Repo.QueryUtils do
  @moduledoc """
  Handy functions for erlang to use when interacting with the DB
  """

  import Ecto.Query

  alias Wocky.Repo

  def get_count(queryable, field \\ :id) do
    queryable
    |> exclude(:preload)
    |> exclude(:select)
    |> exclude(:order_by)
    |> select([..., i], count(field(i, ^field)))
    |> Repo.one!()
  end
end

