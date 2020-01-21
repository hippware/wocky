defmodule WockyAPI.Resolvers.Contact do
  @moduledoc "Resolves GraphQL queries related to friends"

  alias Absinthe.Subscription
  alias Wocky.Account.User
  alias Wocky.Contacts
  alias Wocky.Contacts.Share
  alias Wocky.Events.NearbyStart
  alias Wocky.Location
  alias Wocky.Location.UserLocation
  alias Wocky.Location.UserLocation.Current
  alias Wocky.Notifier
  alias Wocky.Repo
  alias WockyAPI.Endpoint
  alias WockyAPI.Resolvers.Utils

  # -------------------------------------------------------------------
  # Connections

  def get_friends(user, args, %{context: %{current_user: requestor}}),
    do:
      friends_query(
        user,
        args,
        requestor,
        &Contacts.friend_relationships_query/2
      )

  def get_sent_invitations(user, args, %{context: %{current_user: requestor}}),
    do: friends_query(user, args, requestor, &Contacts.sent_invitations_query/2)

  def get_received_invitations(user, args, %{
        context: %{current_user: requestor}
      }),
      do:
        friends_query(
          user,
          args,
          requestor,
          &Contacts.received_invitations_query/2
        )

  defp friends_query(user, args, requestor, query, post_process \\ nil) do
    user
    |> query.(requestor)
    |> Utils.connection_from_query(
      user,
      args,
      desc: :updated_at,
      post_process: post_process
    )
  end

  def get_location_shares(_root, args, %{context: %{current_user: user}}) do
    user
    |> Contacts.get_location_shares_query()
    |> Utils.connection_from_query(user, args, postprocess: &Share.make_shim/1)
  end

  def get_location_sharers(_root, args, %{context: %{current_user: user}}) do
    user
    |> Contacts.get_location_sharers_query()
    |> Utils.connection_from_query(user, args, postprocess: &Share.make_shim/1)
  end

  # -------------------------------------------------------------------
  # Queries

  def get_contact_user(%{contact_id: _} = c, _args, _context) do
    {:ok, Contacts.get_contact_user(c)}
  end

  # Explicitly built map - user should already be in place
  def get_contact_user(%{user: user}, _args, _context), do: {:ok, user}

  # -------------------------------------------------------------------
  # Mutations

  def friend_invite(%{input: input}, %{context: %{current_user: user}}) do
    share_type = Map.get(input, :share_type, :disabled)
    Contacts.make_friends(user, input.user_id, share_type)
  end

  def friend_share_update(%{input: input}, %{context: %{current_user: user}}) do
    opts =
      input[:share_config] ||
        %{}
        |> Map.take([:nearby_distance, :nearby_cooldown])
        |> Enum.into([])

    with :ok <- check_share_opts(opts) do
      case Contacts.update_sharing(
             user,
             input.user_id,
             input.share_type,
             opts
           ) do
        {:ok, friend} ->
          _ = maybe_update_location(input, user)
          {:ok, friend}

        error ->
          error
      end
    end
  end

  defp check_share_opts(opts) do
    min_distance = Confex.get_env(:wocky, :min_nearby_distance)

    cond do
      opts[:nearby_distance] && opts[:nearby_distance] < min_distance ->
        {:error, "nearbyDistance must be at least #{min_distance}"}

      opts[:nearby_cooldown] && opts[:nearby_cooldown] < 0 ->
        {:error, "nearbyCooldown must be at least 0"}

      true ->
        :ok
    end
  end

  defp maybe_update_location(%{location: l}, user) when not is_nil(l),
    do: Location.set_user_location(user, UserLocation.new(l))

  defp maybe_update_location(_args, _user), do: {:ok, :skip}

  def friend_delete(%{input: input}, %{context: %{current_user: user}}) do
    :ok = Contacts.unfriend(user, input.user_id)
    {:ok, true}
  end

  # DEPRECATED
  def live_share_location(%{input: input}, %{context: %{current_user: user}}) do
    case Contacts.update_sharing(user, input.shared_with_id, :always) do
      {:ok, item} ->
        _ = maybe_update_location(input, user)
        {:ok, Share.make_shim(item, input.expires_at)}

      error ->
        error
    end
  end

  # DEPRECATED
  def cancel_location_share(%{input: input}, %{context: %{current_user: user}}) do
    case Contacts.update_sharing(user, input.shared_with_id, :disabled) do
      {:ok, _} ->
        {:ok, true}

      {:error, _} = error ->
        error
    end
  end

  # DEPRECATED
  def cancel_all_location_shares(_args, %{context: %{current_user: user}}) do
    :ok = Contacts.stop_sharing_location(user)

    {:ok, true}
  end

  # -------------------------------------------------------------------
  # Subscriptions

  # Contacts subscription

  def contacts_subscription_topic(user_id),
    do: "contacts_subscription_" <> user_id

  def notify_contact(item, relationship, share_type) do
    notification = %{
      user: item.contact,
      relationship: relationship,
      share_type: share_type,
      created_at: item.created_at
    }

    topic = contacts_subscription_topic(item.user_id)

    Subscription.publish(Endpoint, notification, [{:contacts, topic}])
  end

  # Friends subscription

  def friends_subscription_topic(user_id),
    do: "friends_subscription_" <> user_id

  def notify_friends(user) do
    Repo.transaction(fn ->
      user
      |> Contacts.friend_relationships_query(user)
      |> Repo.stream()
      |> Stream.each(&notify_friend(&1, user))
      |> Stream.run()
    end)
  end

  defp notify_friend(friend_item, user) do
    topic = friends_subscription_topic(friend_item.contact_id)

    Subscription.publish(Endpoint, user, [{:friends, topic}])
  end

  # Location subscription

  def location_subscription_topic(user_id),
    do: "location_subscription_" <> user_id

  def notify_location(user, location) do
    user
    |> Contacts.get_location_share_targets()
    |> Enum.each(&maybe_notify_location(&1, user, location))
  end

  defp maybe_notify_location(
         %{share_type: :always} = share_target,
         user,
         location
       ),
       do: do_notify_location(share_target, user, location)

  defp maybe_notify_location(
         %{share_type: :nearby} = share_target,
         user,
         location
       ) do
    {:ok, target_loc} = Current.get(share_target.contact_id)

    if target_loc &&
         Geocalc.within?(
           share_target.nearby_distance,
           Location.to_point(target_loc),
           Location.to_point(location)
         ) do
      do_notify_location(share_target, user, location)
      maybe_notify_start(user, share_target)
    end
  end

  defp do_notify_location(share_target, user, location) do
    topic = location_subscription_topic(share_target.contact_id)
    data = make_location_data(user, location)

    Subscription.publish(Endpoint, data, [{:shared_locations, topic}])
  end

  defp make_location_data(user, location),
    do: %{user: user, location: location}

  defp maybe_notify_start(
         user,
         %{nearby_last_start_notification: nil} = target
       ),
       do: notify_start(user, target)

  defp maybe_notify_start(user, target) do
    result =
      target.nearby_last_start_notification
      |> DateTime.add(target.nearby_cooldown, :millisecond)
      |> DateTime.compare(DateTime.utc_now())

    if result == :lt, do: notify_start(user, target)
  end

  defp notify_start(user, target) do
    %NearbyStart{
      to: User.hydrate(target.contact_id),
      from: user
    }
    |> Notifier.notify()

    {:ok, _} = Contacts.update_last_start_notification(user, target.contact_id)
    Contacts.refresh_share_cache(user.id)
  end

  def location_catchup(user) do
    result =
      user
      |> Contacts.get_location_sharers()
      |> Enum.reduce([], &build_location_catchup/2)

    {:ok, result}
  end

  defp build_location_catchup(share, acc) do
    with {:ok, location} <- Location.get_current_user_location(share.user) do
      if location do
        [make_location_data(share.user, location) | acc]
      else
        acc
      end
    end
  end
end
