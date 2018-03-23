defmodule WockyAPI.UserResolver do
  @moduledoc "GraphQL resolver for user objects"

  import Ecto.Query

  alias Absinthe.Relay.Connection
  alias Wocky.Bot
  alias Wocky.Repo
  alias Wocky.Repo.ID
  alias Wocky.Roster
  alias Wocky.User
  alias WockyAPI.UtilResolver

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

  def get_bots(_root, args, %{context: %{current_user: user}}) do
    IO.inspect args
    query = Bot.by_relationship_query(user, args[:relationship])

    query
    |> order_by(asc: :updated_at)
    |> Connection.from_query(&Repo.all/1, args)
    |> UtilResolver.add_query(query)
  end
  def get_bots(_, _, _), do: unauthenticated()

  def get_bot_relationships(_root, _args,
                            %{context: %{current_user: user}} = info) do
    {:ok, User.get_bot_relationships(user, info.source.node)}
  end

  def get_contacts(_root, args,
                   %{context: %{current_user: requestor},
                     source: user}) do
      query = case args[:relationship] do
        nil -> Roster.all_contacts_query(user.id, requestor.id, false)
        :friends -> Roster.friends_query(user.id, requestor.id, false)
        :followers -> Roster.followers_query(user.id, requestor.id, false)
        :followees -> Roster.followees_query(user.id, requestor.id, false)
      end

    query
    |> order_by(asc: :updated_at)
    |> Connection.from_query(&Repo.all/1, args)
    |> UtilResolver.add_query(query)
    |> UtilResolver.add_data(:contact_user, user)
  end

  def get_contact_relationship(
    _root, _args, info = %{source: %{node: target_user}}) do

      IO.inspect Map.keys(info)
      IO.inspect info.acc
      IO.inspect info.fields_cache
    {:ok, :friend} #Roster.relationship(target_user, target_user)}
  end

  def get_home_stream(_root, args, %{context: %{current_user: _user}}) do
    Connection.from_list([], args)
  end
  def get_home_stream(_, _, _), do: unauthenticated()

  def get_conversations(_root, args, %{context: %{current_user: _user}}) do
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
