defmodule WockyAPI.Resolvers.User do
  @moduledoc "GraphQL resolver for user objects"

  alias Absinthe.Relay.Connection
  alias Absinthe.Subscription
  alias Wocky.{Account, Repo, Roster, User}
  alias Wocky.Notifier.Push
  alias Wocky.Roster.Item
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
    input = args[:input][:values] |> fix_name(user)

    User.update(user, input)
  end

  def get_first_name(user, _, _), do: {:ok, User.first_name(user)}

  def get_last_name(user, _, _), do: {:ok, User.last_name(user)}

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
      args,
      desc: :updated_at,
      post_process: post_process
    )
  end

  def get_locations(user, args, %{context: %{current_user: user}}) do
    user
    |> User.get_locations_query(args[:device])
    |> Utils.connection_from_query(user, args, order_by: [desc: :captured_at])
  end

  def get_location_events(user, args, %{context: %{current_user: user}}) do
    user
    |> User.get_location_events_query(args[:device])
    |> Utils.connection_from_query(user, args, order_by: [desc: :occurred_at])
  end

  def get_location_events(%Location{} = loc, args, %{
        context: %{current_user: user}
      }) do
    user
    |> User.get_location_events_query(loc)
    |> Utils.connection_from_query(user, args, order_by: [desc: :occurred_at])
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

  def search_users(_root, %{limit: limit}, _info) when limit < 0 do
    {:error, "limit cannot be less than 0"}
  end

  def search_users(_root, %{search_term: search_term} = args, %{
        context: %{current_user: current_user}
      }) do
    limit = args[:limit] || @default_search_results
    {:ok, User.search_by_name(search_term, current_user, limit)}
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
    location = struct(Location, Map.drop(i, [:is_fetch]))

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
           User.start_sharing_location(user, shared_with, input.expires_at),
         {:ok, _} <- maybe_update_location(input, user) do
      {:ok, Repo.preload(share, [:shared_with, :user])}
    else
      nil -> user_not_found(input.shared_with_id)
      error -> error
    end
  end

  defp maybe_update_location(%{location: l}, user) when not is_nil(l),
    do: User.set_location(user, struct(Location, Map.drop(l, [:is_fetch])))

  defp maybe_update_location(_args, _user), do: {:ok, :skip}

  def cancel_location_share(_root, args, %{context: %{current_user: user}}) do
    input = args[:input]

    with %User{} = shared_with <- User.get_user(input.shared_with_id, user) do
      :ok = User.stop_sharing_location(user, shared_with)
    end

    {:ok, true}
  end

  def cancel_all_location_shares(_root, _args, %{context: %{current_user: user}}) do
    :ok = User.stop_sharing_location(user)

    {:ok, true}
  end

  def get_location_shares(_root, args, %{context: %{current_user: user}}) do
    user
    |> User.get_location_shares_query()
    |> Utils.connection_from_query(user, args)
  end

  def get_location_sharers(_root, args, %{context: %{current_user: user}}) do
    user
    |> User.get_location_sharers_query()
    |> Utils.connection_from_query(user, args)
  end

  def notification_subscription_topic(user_id),
    do: "notification_subscription_" <> user_id

  def contacts_subscription_topic(user_id),
    do: "contacts_subscription_" <> user_id

  def friends_subscription_topic(user_id),
    do: "friends_subscription_" <> user_id

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

  def location_catchup(user) do
    result =
      user
      |> User.get_location_sharers()
      |> Enum.reduce([], &build_location_catchup/2)

    {:ok, result}
  end

  defp build_location_catchup(share, acc) do
    location = User.get_current_location(share.user)

    if location do
      [make_location_data(share.user, location) | acc]
    else
      acc
    end
  end

  def notify_location(user, location) do
    user
    |> User.get_location_shares()
    |> Enum.each(&do_notify_location(&1, user, location))
  end

  defp do_notify_location(share, user, location) do
    topic = location_subscription_topic(share.shared_with.id)
    data = make_location_data(user, location)

    Subscription.publish(Endpoint, data, [{:shared_locations, topic}])
  end

  defp make_location_data(user, location),
    do: %{user: user, location: location}

  def hide(_root, _args, _context) do
    # DEPRECATED
    {:ok, true}
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
      {:ok, _} = Roster.set_name(user, other_user, args[:input][:name])
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

  def get_contact_user(%Item{} = c, _args, _context) do
    {:ok,
     c
     |> Repo.preload([:contact])
     |> Map.get(:contact)}
  end

  # Explicitly built map - user should already be in place
  def get_contact_user(x, _args, _context), do: {:ok, x.user}

  defp fix_name(m, user) do
    new_name = do_fix_name(m, user)

    if new_name do
      Map.put_new(m, :name, String.trim(new_name))
    else
      m
    end
  end

  defp do_fix_name(%{first_name: f, last_name: l}, _user),
    do: f <> " " <> l

  defp do_fix_name(%{first_name: f}, user),
    do: f <> " " <> User.last_name(user)

  defp do_fix_name(%{last_name: l}, user),
    do: User.first_name(user) <> " " <> l

  defp do_fix_name(_m, _user), do: nil
end
