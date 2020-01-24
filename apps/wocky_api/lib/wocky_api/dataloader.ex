defmodule WockyAPI.Dataloader do
  @moduledoc """
  Module to encapsulate Dataloader setup
  """

  alias Ecto.Queryable
  alias Wocky.Account.User
  alias Wocky.Contacts
  alias Wocky.Repo

  @spec get(map()) :: Dataloader.t()
  def get(ctx) do
    Dataloader.new()
    |> Dataloader.add_source(Wocky, data(ctx))
  end

  defp data(ctx) do
    Dataloader.Ecto.new(Repo,
      query: &query/2,
      default_params: %{current_user: ctx[:current_user]}
    )
  end

  @spec query(Queryable.t(), map()) :: Queryable.t()
  def query(queryable, %{current_user: current_user}) do
    owner_field =
      case queryable do
        User -> :id
        _ -> :user_id
      end

    Contacts.object_visible_query(queryable, current_user, owner_field)
  end

  def query(queryable, _), do: queryable
end
