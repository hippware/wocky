defmodule WockyAPI.Dataloader do
  @moduledoc """
  Module to encapsulate Dataloader setup
  """

  alias Wocky.Account.User
  alias Wocky.Block
  alias Wocky.Repo

  def get(ctx) do
    Dataloader.new()
    |> Dataloader.add_source(Wocky, data(ctx))
  end

  def data(ctx),
    do:
      Dataloader.Ecto.new(Repo,
        query: &query/2,
        default_params: %{current_user: ctx[:current_user]}
      )

  def query(queryable, %{current_user: current_user}) do
    owner_field =
      case queryable do
        User -> :id
        _ -> :user_id
      end

    Block.object_visible_query(queryable, current_user, owner_field)
  end

  def query(queryable, _), do: queryable
end
