defmodule Wocky.Blocking do
  @moduledoc """
  Helper functions for blocking functionality
  """

  use Wocky.Repo.Model

  alias Wocky.Bot.Item
  alias Wocky.Bot.Share
  alias Wocky.Bot.Subscription
  alias Wocky.Conversation
  alias Wocky.HomeStreamItem
  alias Wocky.RosterItem
  alias Wocky.User

  @blocked_group "__blocked__"
  @blocked_by_group "__blocked_by__"

  def blocked_group, do: @blocked_group
  def blocked_by_group, do: @blocked_by_group

  @doc "blocker initiates a block on blockee"
  @spec block(User.t, User.t) :: :ok
  def block(blocker, blockee) do
    # Set block/blocked groups on roster items
    # and presence subscriptions to 'none'
    write_blocked_items(blocker, blockee)

    # Delete HS message items
    HomeStreamItem.delete(blocker, blockee)
    HomeStreamItem.delete(blockee, blocker)

    # Delete HS bot items for bots owned by blocked user
    # and bot shares of blocked user's bots
    # and content items on owned bots by blocker/blockee
    delete_bot_references(blocker, blockee)
    delete_bot_references(blockee, blocker)

    # Delete conversations and MAM entries between the users
    delete_message_logs(blocker, blockee)
    delete_message_logs(blockee, blocker)
    :ok
  end

  @spec unblock(User.t, User.t) :: :ok
  def unblock(blocker, blockee) do
    write_unblocked_items(blocker, blockee)
  end

  @spec not_blocked_query(Ecto.queryable) :: Ecto.queryable
  def not_blocked_query(query) do
    query
    |> where([..., r], not (@blocked_group in r.groups or
                            @blocked_by_group in r.groups))
  end

  @spec blocked_query(Ecto.queryable) :: Ecto.queryable
  def blocked_query(query) do
    from [..., r] in query, where: @blocked_group in r.groups or
                                   @blocked_by_group in r.groups
  end

  @doc """
  Composable query fragment to filter out objects with owners that are blocking/
  blocked by the supplied user.
  """
  @spec object_visible_query(Ecto.queryable, User.id, atom) :: Ecto.queryable
  def object_visible_query(query, requester_id, owner_field) do
    query
    |> join(:left, [..., o],
            b in RosterItem,
            field(o, ^owner_field) == b.user_id
            and b.contact_id == ^requester_id
            and (@blocked_group in b.groups or
                 @blocked_by_group in b.groups))
    |> where([..., b], is_nil(b.id))
  end

  @spec blocked?(User.id, User.id) :: boolean
  def blocked?(a, b) do
    case RosterItem.get(a, b) do
      nil ->
        false
      %RosterItem{groups: groups} ->
        Enum.member?(groups, @blocked_group) ||
        Enum.member?(groups, @blocked_by_group)
    end
  end

  defp write_blocked_items(blocker, blockee) do
    write_changed_items(
      blocker, blockee,
      [@blocked_group], [@blocked_by_group])
  end

  defp write_unblocked_items(blocker, blockee) do
    write_changed_items(blocker, blockee, [], [])
  end

  defp write_changed_items(a, b, a_groups, b_groups) do
    # Wrap in a transaction so that if two users block each other
    # at the same time, the final result will at least be consistant
    # with one user as the blocker and one as the blockee
    Repo.transaction(
      fn() ->
        [
          %{
            user_id: a.id,
            contact_id: b.id,
            name: "",
            ask: :none,
            subscription: :none,
            groups: a_groups
          },
          %{
            user_id: b.id,
            contact_id: a.id,
            name: "",
            ask: :none,
            subscription: :none,
            groups: b_groups
          }
        ]
        |> Enum.each(&RosterItem.put/1)
      end)
    :ok
  rescue
    _ -> :ok
  end

  defp delete_bot_references(a, b) do
    a
    |> User.get_owned_bots
    |> Enum.each(
      fn(bot) ->
        HomeStreamItem.delete(b, bot)
        Item.delete(bot, b)
        Share.delete(b, bot)
        Subscription.delete(b, bot)
      end)
  end

  defp delete_message_logs(a, b) do
    Conversation.delete_user_pair(a, b)
  end
end
