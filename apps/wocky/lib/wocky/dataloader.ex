defmodule Wocky.Dataloader do
  @moduledoc """
  Module to encapsulate Dataloader setup
  """

  alias Wocky.Repo

  def get do
    Dataloader.new()
    |> Dataloader.add_source(Wocky, data())
  end

  def data, do: Dataloader.Ecto.new(Repo, query: &query/2)

  def query(queryable, _), do: queryable
end
