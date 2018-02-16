defmodule WockyAPI.UserResolver do
  @moduledoc "GraphQL resolver for user objects"

  alias Wocky.Repo
  alias Wocky.User

  def all_users(_root, _args, _info) do
    users = Repo.all(User)
    {:ok, users}
  end

  def current_user(_root, _args, _info) do
  end

  def set_handle(_root, args, _info) do
    IO.puts inspect(args)
    user = Repo.get!(User, args[:id])
    user = Ecto.Changeset.change user, handle: args[:handle]
    case Repo.update(user) do
      {:ok, user} -> {:ok, user}
      {:error, _} -> {:error, "Could not update handle"}
    end
  end
end
