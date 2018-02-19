defmodule WockyAPI.UserResolver do
  @moduledoc "GraphQL resolver for user objects"

  alias Wocky.Repo
  alias Wocky.User

  def get_profile(_root, _args, %{context: %{current_user: current_user}}) do
    {:ok, current_user}
  end

  def update_profile(_root, args, %{context: %{current_user: current_user}}) do
    user = Ecto.Changeset.change current_user, args
    case Repo.update(user) do
      {:ok, user} -> {:ok, user}
      {:error, _} -> {:error, "Could not update handle"}
    end
  end

  def get_contacts(_root, _args, _info) do
  end

  def get_home_stream(_root, _args, _info) do
  end

  def get_conversations(_root, _args, _info) do
  end

  def get_bots(_root, _args, _info) do
  end

  def get_user(_root, args, %{context: %{current_user: _current_user}}) do
    Repo.get(User, args[:id])
  end
end
