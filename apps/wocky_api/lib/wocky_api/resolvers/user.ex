defmodule WockyAPI.Resolvers.User do
  @moduledoc "GraphQL resolver for user objects"

  alias Absinthe.Relay.Connection
  alias Absinthe.Subscription
  alias Wocky.{Account, Push, Repo, Roster, User}
  alias Wocky.Roster.Item
  alias Wocky.User.Location
  alias WockyAPI.{Endpoint, Presence}
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

  def get_contacts(_, %{relationship: :none}, _) do
    {:error, :unsupported}
  end

  def get_contacts(user, args, %{context: %{current_user: requestor}}) do
    with {:query, query} <- contacts_query(user, args, requestor) do
      case query do
        {:error, _} = error -> error
        _ -> Utils.connection_from_query(query, user, args)
      end
    end
  end

  defp contacts_query(user, args, requestor) do
    case args[:relationship] do
      nil ->
        {:query, Roster.friends_query(user, requestor)}

      :friend ->
        {:query, Roster.friends_query(user, requestor)}

      :invited ->
        {:query, Roster.sent_invitations_query(user, requestor)}

      :invited_by ->
        {:query, Roster.received_invitations_query(user, requestor)}

      :follower ->
        {:ok, Connection.from_list([], args)}

      :following ->
        {:ok, Connection.from_list([], args)}
    end
  end

  def get_contact_relationship(_root, _args, %{
        source: %{node: target_user, parent: parent}
      }) do
    {:ok, Roster.relationship(parent, target_user)}
  end

  def get_contact_created_at(_root, _args, %{
        source: %{node: target_user, parent: parent}
      }) do
    item = Roster.get_item(parent, target_user)
    {:ok, item.created_at}
  end

  def get_friends(user, args, %{context: %{current_user: requestor}}),
    do: roster_query(user, args, requestor, &Roster.items_query/2)

  def get_sent_invitations(user, args, %{context: %{current_user: requestor}}),
    do: roster_query(user, args, requestor, &Roster.sent_invitations_query/2)

  def get_received_invitations(user, args, %{
        context: %{current_user: requestor}
      }),
      do:
        roster_query(
          user,
          args,
          requestor,
          &Roster.received_invitations_query/2
        )

  defp roster_query(user, args, requestor, query, post_process \\ nil) do
    user
    |> query.(requestor)
    |> Utils.connection_from_query(
      user,
      [desc: :updated_at],
      post_process,
      args
    )
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
    {:ok, User.search_by_name(search_term, current_user, limit)}
  end

  def get_hidden(user, _args, _info) do
    {hidden, until} = User.hidden_state(user)
    {:ok, %{enabled: hidden, expires: until}}
  end

  def enable_notifications(%{input: i}, %{context: %{current_user: user}}) do
    platform = Map.get(i, :platform)
    dev_mode = Map.get(i, :dev_mode)

    :ok = Push.enable(user, i.device, i.token, platform, dev_mode)
    {:ok, true}
  end

  def disable_notifications(%{input: i}, %{context: %{current_user: user}}) do
    :ok = Push.disable(user, i.device)
    {:ok, true}
  end

  def update_location(_root, %{input: i}, %{context: %{current_user: user}}) do
    location = struct(Location, i)

    with {:ok, _} <- User.set_location(user, location) do
      {:ok, true}
    end
  end

  def get_location_token(_root, _args, %{context: %{current_user: user}}) do
    {:ok, token} = Account.get_location_jwt(user)

    {:ok, %{successful: true, result: token}}
  end

  def live_share_location(_root, args, %{context: %{current_user: user}}) do
    input = args[:input]

    with %User{} = shared_with <- User.get_user(input.shared_with_id, user),
         {:ok, share} <-
           User.start_sharing_location(user, shared_with, input.expires_at) do
      {:ok, Repo.preload(share, :shared_with)}
    else
      nil -> user_not_found(input.shared_with_id)
      {:error, :not_friends} -> {:error, "Can't share location with a stranger"}
      error -> error
    end
  end

  def cancel_location_share(_root, args, %{context: %{current_user: user}}) do
    input = args[:input]

    with %User{} = shared_with <- User.get_user(input.shared_with_id, user) do
      :ok = User.stop_sharing_location(user, shared_with)
    end

    {:ok, true}
  end

  def get_location_shares(_root, args, %{context: %{current_user: user}}) do
    user
    |> User.get_location_shares_query()
    |> Utils.connection_from_query(user, args)
  end

  def notification_subscription_topic(user_id),
    do: "notification_subscription_" <> user_id

  def contacts_subscription_topic(user_id),
    do: "contacts_subscription_" <> user_id

  def friends_subscription_topic(user_id),
    do: "friends_subscription_" <> user_id

  def presence_subscription_topic(user_id),
    do: "presence_subscription_" <> user_id

  def location_subscription_topic(user_id),
    do: "location_subscription_" <> user_id

  def notify_contact(item, relationship) do
    notification = %{
      user: item.contact,
      relationship: relationship,
      name: item.name,
      created_at: item.created_at
    }

    topic = contacts_subscription_topic(item.user_id)

    Subscription.publish(Endpoint, notification, [{:contacts, topic}])
  end

  def notify_friends(user) do
    Repo.transaction(fn ->
      user
      |> Roster.items_query(user)
      |> Repo.stream()
      |> Stream.each(&notify_friend(&1, user))
      |> Stream.run()
    end)
  end

  defp notify_friend(friend_item, user) do
    topic = friends_subscription_topic(friend_item.contact_id)

    Subscription.publish(Endpoint, user, [{:friends, topic}])
  end

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

  def delete(_root, _args, %{context: %{current_user: user}}) do
    User.delete(user.id)
    {:ok, true}
  end

  def user_not_found(id), do: {:error, "User not found: " <> id}

  def make_invite_code(_root, _args, %{context: %{current_user: user}}) do
    code = User.make_invite_code(user)
    {:ok, %{successful: true, result: code}}
  end

  def redeem_invite_code(_root, args, %{context: %{current_user: user}}) do
    result = User.redeem_invite_code(user, args[:input][:code])
    {:ok, %{successful: result, result: result}}
  end

  def invite(_root, args, %{context: %{current_user: user}}) do
    with {:ok, %{relationship: r}} <-
           roster_action(user, args[:input][:user_id], &Roster.invite/2) do
      {:ok, r}
    end
  end

  def unfriend(_root, args, %{context: %{current_user: user}}) do
    with {:ok, _} <-
           roster_action(user, args[:input][:user_id], &Roster.unfriend/2) do
      {:ok, true}
    end
  end

  def name_friend(_root, args, %{context: %{current_user: user}}) do
    with %User{} = other_user <- User.get_user(args[:input][:user_id], user),
         %Item{} <- Roster.get_item(user, other_user) do
      Roster.set_name(user, other_user, args[:input][:name])
      {:ok, true}
    else
      nil -> user_not_found(args[:input][:user_id])
      error -> error
    end
  end

  defp roster_action(%User{id: id}, id, _), do: {:error, "Invalid user"}

  defp roster_action(user, contact_id, roster_fun) do
    with %User{} = contact <- User.get_user(contact_id, user) do
      relationship = roster_fun.(user, contact)
      {:ok, %{relationship: relationship, user: contact}}
    else
      _ -> {:error, "Invalid user"}
    end
  end

  def presence_catchup(user) do
    user
    |> Presence.connect()
    |> Enum.each(&Presence.publish(user.id, &1))
  end

  def get_presence_status(other_user, _args, _context) do
    {:ok, Presence.user_status(other_user)}
  end

  def get_contact_user(%Item{} = c, _args, _context) do
    {:ok,
     c
     |> Repo.preload([:contact])
     |> Map.get(:contact)}
  end

  # Explicitly built map - user should already be in place
  def get_contact_user(x, _args, _context), do: {:ok, x.user}
end
