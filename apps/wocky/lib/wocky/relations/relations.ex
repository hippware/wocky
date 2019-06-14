defmodule Wocky.Relations do
  @moduledoc """
  Code that defines and manipulates relationships between users and bots
  """

  import Ecto.Query

  alias Ecto.Queryable
  alias Wocky.Account.User
  alias Wocky.Block
  alias Wocky.Bots
  alias Wocky.Bots.Bot
  alias Wocky.Bots.Invitation
  alias Wocky.Bots.Subscription
  alias Wocky.Events.GeofenceEvent
  alias Wocky.GeoUtils
  alias Wocky.Location
  alias Wocky.Notifier
  alias Wocky.Relations.Cluster
  alias Wocky.Relations.ClusterSearch
  alias Wocky.Repo
  alias Wocky.Roster

  @type bot_relationship ::
          :owned | :invited | :subscribed | :visitor | :visible

  @type relationship ::
          :visible
          | :owned
          | :subscribed
          | :subscribed_not_owned
          | :invited
          | :visiting

  # ----------------------------------------------------------------------
  # Bot relationships

  @spec get_bot(Bot.id(), User.t(), boolean()) :: Bot.t() | nil
  def get_bot(id, requestor, include_pending \\ false) do
    id
    |> get_bot_query(requestor, include_pending)
    |> Repo.one()
  end

  @spec get_bot_query(Bot.id(), User.t(), boolean()) :: Queryable.t()
  def get_bot_query(id, requestor, include_pending \\ false) do
    id
    |> Bots.get_query(include_pending)
    |> is_visible_query(requestor)
  end

  @spec get_owned_bot(Bot.id(), User.t(), boolean()) :: Bot.t() | nil
  def get_owned_bot(id, %User{id: user_id}, include_pending \\ false) do
    Bot
    |> where(id: ^id, user_id: ^user_id)
    |> Bots.maybe_filter_pending(not include_pending)
    |> Repo.one()
  end

  @doc "Returns all bots that the user owns"
  @spec get_owned_bots(User.t()) :: [Bot.t()]
  def get_owned_bots(user) do
    user
    |> owned_bots_query()
    |> order_by(asc: :updated_at)
    |> Repo.all()
  end

  @spec owned_bots_query(User.t()) :: Queryable.t()
  def owned_bots_query(user) do
    user
    |> Ecto.assoc(:bots)
    |> where(pending: false)
  end

  @spec is_visible_query(Queryable.t(), User.t()) :: Queryable.t()
  def is_visible_query(queryable, user) do
    queryable
    |> Block.object_visible_query(user)
    |> join(
      :left,
      [b, ...],
      invitation in Invitation,
      on: b.id == invitation.bot_id and invitation.invitee_id == ^user.id
    )
    |> join(
      :left,
      [b, ...],
      sub in Subscription,
      on: b.id == sub.bot_id and sub.user_id == ^user.id
    )
    |> where(
      [b, ..., invitation, sub],
      b.user_id == ^user.id or not is_nil(sub.user_id) or
        not is_nil(invitation.user_id)
    )
  end

  @doc "Returns all bots that the user subscribes to"
  @spec get_subscriptions(User.t()) :: [Bot.t()]
  def get_subscriptions(user) do
    Bot
    |> where(pending: false)
    |> join(
      :left,
      [b],
      s in Subscription,
      on: b.id == s.bot_id and s.user_id == ^user.id
    )
    |> where([b, s], not is_nil(s.user_id))
    |> select([:id, :title, :location])
    |> Repo.all()
  end

  @spec active_bots_query(User.t()) :: Queryable.t()
  def active_bots_query(user) do
    user
    |> by_relationship_query(:subscribed, user)
    |> is_visible_query(user)
    |> join(
      :inner,
      [b],
      a in "bot_activity",
      on: b.id == a.bot_id
    )
    |> order_by([..., a], desc: a.visited_at)
  end

  def max_local_bots_search_radius,
    do: Confex.get_env(:wocky, :max_local_bots_search_radius)

  def get_local_bots(user, point_a, point_b, limit) do
    with :ok <- check_area(point_a, point_b) do
      bots =
        user
        |> by_relationship_query(:subscribed, user)
        |> filter_by_location(point_a, point_b)
        |> limit(^limit)
        |> order_by(desc: :created_at)
        |> Repo.all()

      {:ok, bots}
    end
  end

  def get_local_bots_cluster(user, point_a, point_b, lat_divs, lon_divs) do
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

  def filter_by_location(query, point_a, point_b) do
    query
    |> where(
      fragment(
        "location && ST_MakeEnvelope(?, ?, ?, ?, 4326)::geography",
        ^GeoUtils.get_lon(point_a),
        ^GeoUtils.get_lat(point_a),
        ^GeoUtils.get_lon(point_b),
        ^GeoUtils.get_lat(point_b)
      )
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

  @spec can_access?(User.t(), Bot.t()) :: boolean
  def can_access?(user, bot) do
    owns?(user, bot) || Invitation.invited?(bot, user) ||
      Subscription.state(user, bot) != nil
  end

  defp owns?(user, bot), do: user.id == bot.user_id

  @spec get_bot_relationships(User.t(), Bot.t()) :: [bot_relationship()]
  def get_bot_relationships(user, bot) do
    sub = Subscription.get(user, bot)

    [:visible]
    |> maybe_add_rel(bot.user_id == user.id, :owned)
    |> maybe_add_rel(Invitation.invited?(bot, user), :invited)
    |> maybe_add_rel(sub != nil, [:subscribed])
    |> maybe_add_rel(sub != nil && sub.visitor, :visitor)
    |> List.flatten()
  end

  defp maybe_add_rel(list, true, rel), do: [rel | list]
  defp maybe_add_rel(list, false, _rel), do: list

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
    owned_bots_query(user)
  end

  defp by_relationship_query(user, :invited) do
    join(
      Bot,
      :inner,
      [b],
      i in Invitation,
      on: b.id == i.bot_id and i.invitee_id == ^user.id
    )
  end

  defp by_relationship_query(user, :subscribed) do
    join(
      Bot,
      :inner,
      [b],
      s in Subscription,
      on: b.id == s.bot_id and s.user_id == ^user.id
    )
  end

  defp by_relationship_query(user, :subscribed_not_owned) do
    user
    |> by_relationship_query(:subscribed)
    |> where([b, ...], b.user_id != ^user.id)
  end

  defp by_relationship_query(user, :visiting) do
    user
    |> by_relationship_query(:subscribed)
    |> where([..., s], s.visitor)
    |> order_by([..., s], desc: s.visited_at)
  end

  # ----------------------------------------------------------------------
  # Subscriptions

  @spec subscribers_query(Bot.t()) :: [User.t()]
  def subscribers_query(bot) do
    Ecto.assoc(bot, :subscribers)
  end

  @spec subscriber_query(Bot.t(), User.id()) :: Queryable.t()
  def subscriber_query(bot, user_id) do
    bot
    |> Ecto.assoc(:subscribers)
    |> where([..., s], s.user_id == ^user_id)
  end

  @spec visitors_query(Bot.t()) :: Queryable.t()
  def visitors_query(bot) do
    bot
    |> Ecto.assoc(:subscribers)
    |> where([..., s], s.visitor)
    |> order_by([..., s], desc: s.visited_at)
  end

  @spec subscription(Bot.t(), User.t()) :: Subscription.state()
  def subscription(bot, user) do
    Subscription.state(user, bot)
  end

  @spec subscribe(Bot.t(), User.t()) :: :ok | {:error, :permission_denied}
  def subscribe(bot, user) do
    if Roster.self_or_friend?(user.id, bot.user_id) do
      Location.add_subscription(user, bot)
      Subscription.put(user, bot)
    else
      {:error, :permission_denied}
    end
  end

  @spec unsubscribe(Bot.t(), User.t()) :: :ok | {:error, any}
  def unsubscribe(bot, user) do
    Location.exit_bot(user, bot, "unsubscribe")
    Location.remove_subscription(user, bot)
    Subscription.delete(user, bot)
  end

  @spec visit(Bot.t(), User.t(), boolean()) :: :ok
  def visit(bot, user, notify) do
    Subscription.visit(user, bot)
    if notify, do: send_visit_notifications(user, bot, :enter)
    :ok
  end

  @spec depart(Bot.t(), User.t(), boolean()) :: :ok
  def depart(bot, user, notify) do
    Subscription.depart(user, bot)
    if notify, do: send_visit_notifications(user, bot, :exit)
    :ok
  end

  defp send_visit_notifications(visitor, bot, bot_event) do
    event = %GeofenceEvent{
      from: visitor,
      bot: bot,
      event: bot_event
    }

    bot
    |> notification_recipients(visitor)
    |> Enum.each(&do_send_visit_notification(&1, event))
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
      |> Roster.friends_query(sender)
      |> Repo.all()
      |> MapSet.new()
      |> MapSet.put(sender)

    subscribers
    |> MapSet.intersection(friends)
    |> MapSet.to_list()
  end

  defp do_send_visit_notification(subscriber, event),
    do: Notifier.notify(%GeofenceEvent{event | to: subscriber})
end
