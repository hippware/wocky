defmodule WockyAPI.Resolvers.User do
  @moduledoc "GraphQL resolver for user objects"

  alias Wocky.{Conversation, JID, Message, Roster, User}
  alias Wocky.User.Location
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
    |> Utils.connection_from_query(user, [desc: :captured_at], args)
  end

  def get_location_events(user, args, %{context: %{current_user: user}}) do
    user
    |> User.get_location_events_query(args[:device])
    |> Utils.connection_from_query(user, [desc: :occurred_at], args)
  end

  def get_location_events(%Location{} = loc, args, %{
        context: %{current_user: user}
      }) do
    user
    |> User.get_location_events_query(loc)
    |> Utils.connection_from_query(user, [desc: :occurred_at], args)
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

  def get_hidden(user, _args, _info) do
    {hidden, until} = User.hidden_state(user)
    {:ok, %{enabled: hidden, expires: until}}
  end

  def update_location(_root, %{input: i}, %{context: %{current_user: user}}) do
    params = Map.put(i, :resource, i[:device] || i[:resource])
    location = struct(Location, params)

    with {:ok, _} <- User.set_location(user, location) do
      {:ok, true}
    end
  end

  def notification_subscription_topic(user_id),
    do: "notification_subscription_" <> user_id

  def contacts_subscription_topic(user_id),
    do: "contacts_subscription_" <> user_id

  def notify_contact(item, relationship) do
    notification = %{
      user: item.contact,
      relationship: map_relationship(relationship)
    }
    topic = contacts_subscription_topic(item.user_id)

    Subscription.publish(Endpoint, notification, [{:contacts, topic}])
  end

  def has_used_geofence(_root, _args, _context), do: {:ok, true}

  def hide(_root, %{input: input}, %{context: %{current_user: user}}) do
    with {:ok, _} <- do_hide(user, input) do
      {:ok, true}
    end
  end

  defp do_hide(user, input) do
    param =
      case {input[:enable], input[:expire]} do
        {false, _} -> false
        {true, nil} -> true
        {true, expire} -> expire
      end

    User.hide(user, param)
  end

  def get_messages(_root, args, %{context: %{current_user: user}}) do
    with {:ok, query} <- get_messages_query(args[:other_user], user) do
      query
      |> Utils.connection_from_query(user, [desc: :id], &Message.fix/1, args)
    end
  end

  defp get_messages_query(nil, requestor),
    do: {:ok, Message.get_query(requestor)}

  defp get_messages_query(other_user_id, requestor) do
    with %User{} = other_user <- User.get_user(other_user_id, requestor) do
      {:ok, Message.get_query(requestor, other_user)}
    else
      _ -> user_not_found(other_user_id)
    end
  end

  def delete(_root, _args, %{context: %{current_user: user}}) do
    User.delete(user.id)
    {:ok, true}
  end

  defp user_not_found(id), do: {:error, "User not found: " <> id}

  def make_invite_code(_root, _args, %{context: %{current_user: user}}) do
    code = User.make_invite_code(user)
    {:ok, %{successful: true, result: code}}
  end

  def redeem_invite_code(_root, args, %{context: %{current_user: user}}) do
    result = User.redeem_invite_code(user, args[:input][:code])
    {:ok, %{successful: result, result: result}}
  end

  def follow(_root, args, %{context: %{current_user: user}}),
    do: roster_action(user, args[:input][:user_id], &Roster.become_follower/2)

  def unfollow(_root, args, %{context: %{current_user: user}}),
    do: roster_action(user, args[:input][:user_id], &Roster.stop_following/2)

  defp roster_action(%User{id: id}, id, _), do: {:error, "Invalid user"}

  defp roster_action(user, contact_id, roster_fun) do
    with %User{} = contact <- User.get_user(contact_id, user) do
      relationship = roster_fun.(user.id, contact.id)
      {:ok, %{relationship: map_relationship(relationship), user: contact}}
    else
      _ -> {:error, "Invalid user"}
    end
  end

  defp map_relationship(:followee), do: :following
  defp map_relationship(r), do: r
end
