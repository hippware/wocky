defmodule Wocky.Relations do
  @moduledoc """
  Code that defines and manipulates relationships between users and bots
  """

  import Ecto.Query

  alias Ecto.Queryable
  alias Wocky.Account.User
  alias Wocky.Bots.Bot
  alias Wocky.Bots.Invitation
  alias Wocky.Bots.Subscription
  alias Wocky.Repo

  @type bot_relationship ::
          :owned | :invited | :subscribed | :visitor | :visible

  # ----------------------------------------------------------------------
  # Bot relationships

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
end
