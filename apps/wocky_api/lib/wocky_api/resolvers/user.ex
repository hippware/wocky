defmodule WockyAPI.Resolvers.User do
  @moduledoc "GraphQL resolver for user objects"

  import Ecto.Query

  alias Absinthe.Subscription
  alias Wocky.Bot
  alias Wocky.Conversation
  alias Wocky.HomeStream
  alias Wocky.JID
  alias Wocky.Repo
  alias Wocky.Roster
  alias Wocky.User
  alias Wocky.User.Location
  alias WockyAPI.Endpoint
  alias WockyAPI.Resolvers.Utils

  @default_search_results 50

  def get_current_user(_root, _args, %{context: %{current_user: user}}) do
    {:ok, user}
  end

  def get_current_user(_root, _args, _info) do
    {:error, "This operation requires an authenticated user"}
  end

  def update_user(_root, args, %{context: %{current_user: user}}) do
    input = args[:input][:values]

    User.update(user, input)
  end

  def get_contacts(user, args, %{context: %{current_user: requestor}}) do
    query =
      case args[:relationship] do
        nil -> Roster.all_contacts_query(user.id, requestor.id, false)
        :friend -> Roster.friends_query(user.id, requestor.id, false)
        :follower -> Roster.followers_query(user.id, requestor.id, false)
        :following -> Roster.followees_query(user.id, requestor.id, false)
      end

    Utils.connection_from_query(query, user, args)
  end

  def get_contact_relationship(_root, _args, %{
        source: %{node: target_user, parent: parent}
      }) do
    {:ok, Roster.relationship(parent.id, target_user.id)}
  end

  def get_home_stream(user, args, _info) do
    user.id
    |> HomeStream.get_query()
    |> Utils.connection_from_query(user, args)
  end

  def get_conversations(user, args, _info) do
    user.id
    |> Conversation.with_user()
    |> Utils.connection_from_query(user, args)
  end

  def get_conversation_user(conversation, _args, _info) do
    {:ok, User.get_by_jid(JID.from_binary(conversation.other_jid))}
  end

  def get_locations(user, args, %{context: %{current_user: user}}) do
    user
    |> User.get_locations_query(args[:device])
    |> Utils.connection_from_query(user, args)
  end

  def get_location_events(user, args, %{context: %{current_user: user}}) do
    user
    |> User.get_location_events_query(args[:device])
    |> Utils.connection_from_query(user, args)
  end

  def get_location_events(%Location{} = loc, args, %{
        context: %{current_user: user}
      }) do
    user
    |> User.get_location_events_query(loc)
    |> Utils.connection_from_query(user, args)
  end

  def get_user(_root, %{id: id}, %{context: %{current_user: %{id: id} = u}}) do
    {:ok, u}
  end

  def get_user(_root, %{id: id}, %{context: %{current_user: current_user}}) do
    with %User{} = user <- User.get_user(id, current_user) do
      {:ok, user}
    else
      _ -> user_not_found(id)
    end
  end

  # This is kind of dumb - an anonymous user can see more than an authenticated
  # but blocked user...
  def get_user(_root, %{id: id}, _info) do
    with %User{} = user <- User.get_user(id) do
      {:ok, user}
    else
      _ -> user_not_found(id)
    end
  end

  def search_users(_root, %{limit: limit}, _info) when limit < 0 do
    {:error, "limit cannot be less than 0"}
  end

  def search_users(_root, %{search_term: search_term} = args, %{
        context: %{current_user: current_user}
      }) do
    limit = args[:limit] || @default_search_results
    {:ok, User.search_by_name(search_term, current_user.id, limit)}
  end

  def update_location(_root, args, %{context: %{current_user: user}}) do
    location = args[:input]
    update_location(location, user)
  end

  def update_location(location, user) do
    device = location[:device] || location[:resource]
    with :ok <-
           User.set_location(
             user,
             device,
             location[:lat],
             location[:lon],
             location[:accuracy]
           ) do
      {:ok, true}
    end
  end

  def home_stream_subscription_topic(user_id) do
    "home_stream_subscription_" <> user_id
  end

  def notify_home_stream(item, action) do
    notification = %{item: item, action: action}
    topic = home_stream_subscription_topic(item.user_id)

    Subscription.publish(Endpoint, notification, [{:home_stream, topic}])
  end

  def has_used_geofence(_root, _args, %{context: %{current_user: user}}) do
    {:ok,
     user
     |> Bot.related_geofence_bots_query()
     |> select([b], count(1))
     |> Repo.one!()
     |> Kernel.!=(0)}
  end

  defp user_not_found(id), do: {:error, "User not found: " <> id}
end
