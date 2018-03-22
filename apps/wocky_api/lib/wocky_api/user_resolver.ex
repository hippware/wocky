defmodule WockyAPI.UserResolver do
  @moduledoc "GraphQL resolver for user objects"

  alias Absinthe.Relay.Connection
  alias Wocky.Repo
  alias Wocky.Repo.ID
  alias Wocky.User

  def get_current_user(_root, _args, %{context: %{current_user: user}}) do
    {:ok, user}
  end
  def get_current_user(_, _, _), do: unauthenticated()

  def get_private_user_data(_root, _args, %{context: %{current_user: user}}) do
    {:ok, user}
  end
  def get_private_user_data(_, _, _), do: unauthenticated()

  def update_user(_root, args, %{context: %{current_user: user}}) do
    input = args[:input]

    case User.update(user, input) do
      {:ok, user} ->
        {:ok, %{user: user, private_user_data: user}}

      {:error, _} ->
        {:error, "Could not update user"}
    end
  end
  def update_user(_, _, _), do: unauthenticated()

  def get_owned_bots(_root, args, %{context: %{current_user: user}}) do
    user
    |> User.get_owned_bots()
    |> Connection.from_list(args)
  end

  def get_owned_bots_total_count(_, _, %{context: %{current_user: user}}) do
    {:ok, User.bot_count(user)}
  end

  def get_bots(_root, args, %{context: %{current_user: _user}}) do
    Connection.from_list([], args)
  end
  def get_bots(_, _, _), do: unauthenticated()

  def get_bots_total_count(_root, _args, %{context: %{current_user: _user}}) do
    {:ok, 0}
  end

  def get_bot_relationship(_root, _args, %{context: %{current_user: _user}}) do
    {:ok, :visible}
  end

  def get_contacts(_root, args, _info) do
    Connection.from_list([], args)
  end

  def get_contacts_total_count(_, _args, %{context: %{current_user: _user}}) do
    {:ok, 0}
  end

  def get_contacts_follower_count(_, _, %{context: %{current_user: _user}}) do
    {:ok, 0}
  end

  def get_contacts_following_count(_, _, %{context: %{current_user: _user}}) do
    {:ok, 0}
  end

  def get_contact_relationship(_, _args, %{context: %{current_user: _user}}) do
    {:ok, :friend}
  end

  def get_home_stream(_root, args, _info) do
    Connection.from_list([], args)
  end
  def get_home_stream(_, _, _), do: unauthenticated()

  def get_conversations(_root, args, _info) do
    Connection.from_list([], args)
  end
  def get_conversations(_, _, _), do: unauthenticated()

  def get_user(_root, args, %{context: %{current_user: _current_user}}) do
    if ID.valid?(args[:id]) do
      case Repo.get(User, args[:id]) do
        nil -> {:error, "User not found: " <> args[:id]}
        user -> {:ok, user}
      end
    else
      {:error, "Invalid user id: " <> args[:id]}
    end
  end
  def get_user(_, _, _), do: unauthenticated()


  defp unauthenticated,
  do: {:error, "Query not allowed for unauthenticated users"}
end
