defmodule Wocky.Relation do
  @moduledoc """
  Code that defines and manipulates relationships between users and bots
  """

  use Wocky.Context
  use Elixometer

  import Ecto.Query

  alias Geo.Point
  alias Wocky.Account.User
  alias Wocky.Contacts
  alias Wocky.Events.GeofenceEvent
  alias Wocky.GeoUtils
  alias Wocky.Location
  alias Wocky.Notifier
  alias Wocky.POI
  alias Wocky.POI.Bot
  alias Wocky.Relation.Cluster
  alias Wocky.Relation.ClusterSearch
  alias Wocky.Relation.Invitation
  alias Wocky.Relation.Subscription

  @type relationship ::
          :visible
          | :owned
          | :subscribed
          | :subscribed_not_owned
          | :invited
          | :visiting

  # ----------------------------------------------------------------------
  # Get bots by relationship to user

  @spec get_bot(Bot.id(), User.tid(), boolean()) :: Bot.t() | nil
  def get_bot(id, requestor, include_pending \\ false) do
    id
    |> get_bot_query(requestor, include_pending)
    |> Repo.one()
  end

  @spec get_bot_query(Bot.id(), User.tid(), boolean()) :: Queryable.t()
  def get_bot_query(id, requestor, include_pending \\ false) do
    id
    |> POI.get_query(include_pending)
    |> is_visible_query(requestor)
  end

  @spec get_owned_bot(Bot.id(), User.t(), boolean()) :: Bot.t() | nil
  def get_owned_bot(id, user, include_pending \\ false) do
    user
    |> get_owned_bot_query(id)
    |> POI.maybe_filter_pending(not include_pending)
    |> Repo.one()
  end

  defp get_owned_bot_query(user, id) do
    from b in assoc(user, :bots),
      where: b.id == ^id
  end

  @doc "Returns all bots that the user owns"
  @spec get_owned_bots(User.t()) :: [Bot.t()]
  def get_owned_bots(user) do
    Repo.all(
      from b in by_relationship_query(user, :owned),
        order_by: [asc: :updated_at]
    )
  end

  @doc "Returns all bots that the user subscribes to"
  @spec get_subscribed_bots(User.tid()) :: [Bot.t()]
  def get_subscribed_bots(user) do
    Repo.all(
      from b in Bot,
        where: b.pending == false,
        left_join: s in Subscription,
        on: b.id == s.bot_id and s.user_id == ^User.id(user),
        where: not is_nil(s.user_id),
        select: [:id, :title, :location]
    )
  end

  @doc """
  Returns all active bots for a user.

  An "active" bot is a bot that the user subscribes to that currently has
  visitors. The list is sorted by the enter time of the most recent visitor
  to each bot.
  """
  @spec get_active_bots(User.t()) :: [Bot.t()]
  def get_active_bots(user) do
    user
    |> active_bots_query()
    |> Repo.all()
  end

  @spec active_bots_query(User.t()) :: Queryable.t()
  def active_bots_query(user) do
    from b in visible_bots(user),
      inner_join: a in "bot_activity",
      on: b.id == a.bot_id,
      order_by: [desc: a.visited_at]
  end

  defp visible_bots(user) do
    user
    |> by_relationship_query(:subscribed, user)
    |> is_visible_query(user)
  end

  @spec max_local_bots_search_radius :: float()
  def max_local_bots_search_radius,
    do: Confex.get_env(:wocky, :max_local_bots_search_radius)

  @spec get_local_bots(User.t(), Point.t(), Point.t(), pos_integer()) ::
          {:ok, [Bot.t()]} | {:error, :area_too_large}
  def get_local_bots(user, point_a, point_b, limit) do
    with :ok <- check_area(point_a, point_b) do
      bots =
        Repo.all(
          from local_bots(user, point_a, point_b),
            order_by: [desc: :created_at],
            limit: ^limit
        )

      {:ok, bots}
    end
  end

  defp local_bots(user, point_a, point_b) do
    user
    |> by_relationship_query(:subscribed, user)
    |> filter_by_location(point_a, point_b)
  end

  @spec get_local_bots_clustered(
          User.tid(),
          Point.t(),
          Point.t(),
          pos_integer(),
          pos_integer()
        ) :: {:ok, [Bot.t()], [Cluster.t()]} | {:error, :area_too_large}
  def get_local_bots_clustered(user, point_a, point_b, lat_divs, lon_divs) do
    with :ok <- check_area(point_a, point_b) do
      results = ClusterSearch.search(point_a, point_b, lat_divs, lon_divs, user)

      {bots, clusters} =
        Enum.split_with(results, fn
          %Bot{} -> true
          %Cluster{} -> false
        end)

      {:ok, bots, clusters}
    end
  end

  @spec filter_by_location(Queryable.t(), Point.t(), Point.t()) :: Queryable.t()
  def filter_by_location(query, point_a, point_b) do
    from query,
      where:
        fragment(
          "location && ST_MakeEnvelope(?, ?, ?, ?, 4326)::geography",
          ^GeoUtils.get_lon(point_a),
          ^GeoUtils.get_lat(point_a),
          ^GeoUtils.get_lon(point_b),
          ^GeoUtils.get_lat(point_b)
        )
  end

  defp check_area(point_a, point_b) do
    diagonal =
      Geocalc.distance_between(point_a.coordinates, point_b.coordinates)

    if diagonal > max_local_bots_search_radius() do
      {:error, :area_too_large}
    else
      :ok
    end
  end

  @spec get_bots_by_relationship(User.t(), relationship(), User.t() | nil) ::
          [Bot.t()]
  def get_bots_by_relationship(user, relationship, requestor \\ nil) do
    user
    |> by_relationship_query(relationship, requestor)
    |> Repo.all()
  end

  @spec by_relationship_query(User.t(), relationship(), User.t() | nil) ::
          Queryable.t()
  def by_relationship_query(user, rel, user) do
    by_relationship_query(user, rel)
  end

  def by_relationship_query(user, rel, requestor) do
    user
    |> by_relationship_query(rel)
    |> is_visible_query(requestor)
  end

  defp by_relationship_query(user, :visible) do
    is_visible_query(Bot, user)
  end

  defp by_relationship_query(user, :owned) do
    from b in assoc(user, :bots),
      where: b.pending == false
  end

  defp by_relationship_query(user, :invited) do
    assoc(user, :bot_invitations)
  end

  defp by_relationship_query(user, :subscribed) do
    assoc(user, :bot_subscriptions)
  end

  defp by_relationship_query(user, :subscribed_not_owned) do
    from b in by_relationship_query(user, :subscribed),
      where: b.user_id != ^user.id
  end

  defp by_relationship_query(user, :visiting) do
    from [..., s] in by_relationship_query(user, :subscribed),
      where: s.visitor,
      order_by: [desc: s.visited_at]
  end

  @spec is_visible_query(Queryable.t(), User.t()) :: Queryable.t()
  def is_visible_query(queryable, user) do
    user_id = User.id(user)

    from b in Contacts.object_visible_query(queryable, user),
      left_join: invitation in Invitation,
      on: b.id == invitation.bot_id and invitation.invitee_id == ^user_id,
      left_join: sub in Subscription,
      on: b.id == sub.bot_id and sub.user_id == ^user_id,
      where:
        b.user_id == ^user_id or not is_nil(sub.user_id) or
          not is_nil(invitation.user_id)
  end

  # ----------------------------------------------------------------------
  # Get users by relationship to bot

  @spec get_subscribers(Bot.t()) :: [User.t()]
  def get_subscribers(bot) do
    bot |> subscribers_query() |> Repo.all()
  end

  @spec subscribers_query(Bot.t()) :: Queryable.t()
  def subscribers_query(bot) do
    assoc(bot, :subscribers)
  end

  @spec get_subscriber(Bot.t(), User.tid()) :: User.t() | nil
  def get_subscriber(bot, user) do
    bot |> subscriber_query(user) |> Repo.one()
  end

  @spec subscriber_query(Bot.t(), User.tid()) :: Queryable.t()
  def subscriber_query(bot, user) do
    from [..., s] in assoc(bot, :subscribers),
      where: s.user_id == ^User.id(user)
  end

  @spec get_visitors(Bot.t()) :: [User.t()]
  def get_visitors(bot) do
    bot |> visitors_query() |> Repo.all()
  end

  @spec visitors_query(Bot.t()) :: Queryable.t()
  def visitors_query(bot) do
    from [..., s] in assoc(bot, :subscribers),
      where: s.visitor,
      order_by: [desc: s.visited_at]
  end

  @doc false
  @spec notification_recipients(Bot.t(), User.t()) :: [User.t()]
  def notification_recipients(bot, sender) do
    # TODO We might be able to reduce the round trips to the database
    # by combining the friends query and the subscriber query. That would
    # result in an ugly join, so I am doing the easy thing for now.
    bot = Repo.preload(bot, [:subscribers])
    subscribers = MapSet.new(bot.subscribers)

    friends =
      sender
      |> Contacts.friends_query(sender)
      |> Repo.all()
      |> MapSet.new()
      |> MapSet.put(sender)

    subscribers
    |> MapSet.intersection(friends)
    |> MapSet.to_list()
  end

  # ----------------------------------------------------------------------
  # Query a user's relationship to a bot

  @spec owned?(User.tid(), Bot.t()) :: boolean()
  def owned?(user, bot), do: User.id(user) == bot.user_id

  @spec invited?(User.tid(), Bot.t()) :: boolean()
  def invited?(user, bot) do
    Repo.exists?(
      from i in Invitation,
        where: i.bot_id == ^bot.id and i.invitee_id == ^User.id(user)
    )
  end

  @spec subscribed?(User.tid(), Bot.t()) :: boolean()
  def subscribed?(user, bot) do
    Repo.exists?(get_subscription_query(user, bot))
  end

  @spec visiting?(User.tid(), Bot.t()) :: boolean()
  def visiting?(user, bot) do
    sub = get_subscription(user, bot)

    sub != nil && sub.visitor
  end

  @spec visible?(User.tid(), Bot.t()) :: boolean()
  def visible?(user, bot) do
    owned?(user, bot) || invited?(user, bot) || subscribed?(user, bot)
  end

  @spec get_bot_relationships(User.tid(), Bot.t()) :: [relationship()]
  def get_bot_relationships(user, bot) do
    sub = get_subscription(user, bot)

    [:visible]
    |> maybe_add_rel(owned?(user, bot), :owned)
    |> maybe_add_rel(invited?(user, bot), :invited)
    |> maybe_add_rel(sub != nil, [:subscribed])
    |> maybe_add_rel(sub != nil && sub.visitor, :visitor)
    |> List.flatten()
  end

  defp maybe_add_rel(list, true, rel), do: [rel | list]
  defp maybe_add_rel(list, false, _rel), do: list

  # ----------------------------------------------------------------------
  # Subscriptions

  @spec get_subscription(User.tid(), Bot.t()) :: Subscription.t() | nil
  def get_subscription(user, bot) do
    Repo.one(get_subscription_query(user, bot))
  end

  defp get_subscription_query(user, bot) do
    from s in Subscription,
      where: s.user_id == ^User.id(user) and s.bot_id == ^bot.id
  end

  @spec subscribe(User.tid(), Bot.t()) :: :ok | {:error, any()}
  def subscribe(user, bot) do
    contact_id = bot.user_id

    if Contacts.self?(user, contact_id) || Contacts.friend?(user, contact_id) do
      with {:ok, _} <- do_subscribe(user, bot) do
        update_counter("bot.subscription.subscribe", 1)

        :ok
      end
    else
      {:error, :permission_denied}
    end
  end

  defp do_subscribe(user, bot) do
    %Subscription{}
    |> Subscription.changeset(%{user_id: User.id(user), bot_id: bot.id})
    |> Repo.insert(
      on_conflict: [
        set: [updated_at: DateTime.utc_now()]
      ],
      conflict_target: [:user_id, :bot_id]
    )
  end

  @spec unsubscribe(User.tid(), Bot.t()) :: :ok | {:error, any}
  def unsubscribe(user, bot) do
    if User.id(user) != bot.user_id do
      do_unsubscribe(user, bot)
    else
      {:error, :denied}
    end
  end

  defp do_unsubscribe(user, bot) do
    Location.exit_bot(user, bot, "unsubscribe")

    {count, _} =
      Repo.delete_all(
        from s in Subscription,
          where: s.user_id == ^User.id(user) and s.bot_id == ^bot.id
      )

    update_counter("bot.subscription.unsubscribe", count)

    :ok
  end

  @spec visit(User.tid(), Bot.t(), boolean()) :: :ok
  def visit(user, bot, notify? \\ false) do
    update_counter("bot.geofence.visit", 1)
    do_visit(user, bot, :enter, notify?)
  end

  @spec depart(User.tid(), Bot.t(), boolean()) :: :ok
  def depart(user, bot, notify? \\ false) do
    update_counter("bot.geofence.depart", 1)
    do_visit(user, bot, :exit, notify?)
  end

  defp do_visit(user, bot, event, notify?) do
    now = DateTime.utc_now()
    enter? = event == :enter

    timestamps =
      case event do
        :enter -> [visited_at: now, departed_at: nil]
        :exit -> [departed_at: now]
      end

    Subscription
    |> where(user_id: ^User.id(user), bot_id: ^bot.id)
    |> Repo.update_all(set: [visitor: enter?, updated_at: now] ++ timestamps)

    if notify?, do: send_visit_notifications(user, bot, event)

    :ok
  end

  defp send_visit_notifications(visitor, bot, bot_event) do
    visitor = User.hydrate(visitor)

    event = %GeofenceEvent{
      from: visitor,
      bot: bot,
      event: bot_event
    }

    bot
    |> notification_recipients(visitor)
    |> Enum.each(&do_send_visit_notification(&1, event))
  end

  defp do_send_visit_notification(subscriber, event),
    do: Notifier.notify(%GeofenceEvent{event | to: subscriber})

  @spec delete_subscriptions_for_owned_bots(User.tid(), User.tid()) :: :ok
  def delete_subscriptions_for_owned_bots(bot_owner, user) do
    {count, _} =
      Repo.delete_all(
        from s in Subscription,
          inner_join: b in assoc(s, :bot),
          where:
            b.user_id == ^User.id(bot_owner) and s.user_id == ^User.id(user)
      )

    update_counter("bot.subscription.unsubscribe", count)

    :ok
  end

  # ----------------------------------------------------------------------
  # Invitations

  @spec invite(User.tid(), Bot.t(), User.tid()) ::
          {:ok, Invitation.t()} | {:error, any()}
  def invite(invitee, bot, user) do
    if owned?(user, bot) && Contacts.friend?(invitee, user) do
      %Invitation{}
      |> Invitation.changeset(%{
        user_id: User.id(user),
        bot_id: bot.id,
        invitee_id: User.id(invitee)
      })
      |> Repo.insert(
        returning: true,
        on_conflict: [set: [updated_at: DateTime.utc_now()]],
        conflict_target: [:user_id, :bot_id, :invitee_id]
      )
    else
      {:error, :permission_denied}
    end
  end

  @spec get_invitation(Invitation.id(), User.tid()) :: Invitation.t() | nil
  def get_invitation(id, invitee) do
    Repo.one(
      from i in assoc(invitee, :received_invitations),
        where: i.id == ^id,
        preload: [:bot, :invitee]
    )
  end

  @spec respond(Invitation.t(), boolean(), User.tid()) ::
          {:ok, Invitation.t()} | {:error, any()}
  def respond(invitation, accepted?, invitee) do
    if User.id(invitee) == invitation.invitee_id do
      with {:ok, result} <- do_respond(invitation, accepted?),
           :ok <- maybe_subscribe(invitation, accepted?) do
        {:ok, result}
      end
    else
      {:error, :permission_denied}
    end
  end

  defp do_respond(invitation, accepted?) do
    invitation
    |> Invitation.changeset(%{accepted: accepted?})
    |> Repo.update()
  end

  defp maybe_subscribe(_, false), do: :ok

  defp maybe_subscribe(invitation, true) do
    subscribe(invitation.invitee, invitation.bot)
  end

  @spec delete_invitation(User.t(), User.tid()) :: :ok
  def delete_invitation(user, invitee) do
    Repo.delete_all(
      from i in assoc(user, :sent_invitations),
        where: i.invitee_id == ^User.id(invitee)
    )

    :ok
  end
end
