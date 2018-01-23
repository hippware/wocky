defmodule RosterHelper do
  alias Wocky.Repo.Factory

  @doc "Make user1 and user2 friends"
  def make_friends(user1, user2) do
    Factory.insert(
      :roster_item,
      user_id: user1.id,
      contact_id: user2.id
    )

    Factory.insert(
      :roster_item,
      user_id: user2.id,
      contact_id: user1.id
    )
  end

  @doc "Makes user1 a follower of user2"
  def follow(follower, followee) do
    Factory.insert(
      :roster_item,
      user_id: follower.id,
      contact_id: followee.id,
      subscription: :to
    )

    Factory.insert(
      :roster_item,
      user_id: followee.id,
      contact_id: follower.id,
      subscription: :from
    )
  end
end
