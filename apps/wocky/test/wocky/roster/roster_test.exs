defmodule Wocky.RosterTest do
  use Wocky.DataCase, async: true

  alias Faker.Lorem
  alias Faker.Name
  alias Wocky.Block
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.Roster
  alias Wocky.Roster.Item, as: RosterItem
  alias Wocky.User

  setup do
    # A user with 5 contacts in a randomised subset of 5 groups
    user = Factory.insert(:user)

    contacts = for _ <- 1..5, do: Factory.insert(:user)

    groups = for _ <- 1..5, do: Lorem.word()
    roster_pairs = Enum.map(contacts, &insert_friend_pair(user, &1, groups))

    rosterless_user = Factory.insert(:user)

    follower = Factory.insert(:user)
    followee = Factory.insert(:user)

    blocked_viewer = Factory.insert(:user)
    Block.block(follower, blocked_viewer)
    Block.block(blocked_viewer, followee)

    insert_follower_pair(follower, followee)

    system_user = Factory.insert(:user, roles: [User.system_role()])
    insert_friend_pair(user, system_user, [Lorem.word()])

    nil_handle_user = Factory.insert(:user, handle: nil)
    insert_friend_pair(nil_handle_user, user, [Lorem.word()])

    visible_contacts = Enum.sort([system_user | contacts])

    {:ok,
     user: user,
     all_contacts: Enum.sort([system_user, nil_handle_user | contacts]),
     visible_contacts: visible_contacts,
     contact: hd(visible_contacts),
     roster_pairs: roster_pairs,
     roster_pair: hd(roster_pairs),
     rosterless_user: rosterless_user,
     follower: follower,
     followee: followee,
     blocked_viewer: blocked_viewer,
     groups: groups,
     system_user: system_user,
     nil_handle_user: nil_handle_user}
  end

  describe "get/1" do
    test "should return all roster items for a user", ctx do
      contacts =
        ctx.user.id
        |> Roster.get()
        |> Enum.map(&Map.get(&1, :contact))
        |> Enum.sort()

      assert contacts == ctx.all_contacts
    end

    test "should return an empty list for a user with no roster items", ctx do
      assert Roster.get(ctx.rosterless_user.id) == []
    end

    test "should return an empty list for a non-existant user" do
      assert Roster.get(ID.new()) == []
    end
  end

  describe "get/2" do
    test "should return the roster item for the specified contact", ctx do
      Enum.map(ctx.all_contacts, fn c ->
        assert ctx.user.id |> Roster.get(c.id) |> Map.get(:contact) == c
      end)
    end
  end

  describe "get_pair/2" do
    test "should return the pair of roster items with the first one first",
         ctx do
      {a, b} = ctx.roster_pair

      assert Roster.get_pair(ctx.user.id, ctx.contact.id) == {a, b}
      assert Roster.get_pair(ctx.contact.id, ctx.user.id) == {b, a}
    end

    test "should return nil where no relationship exists", ctx do
      refute Roster.get_pair(ctx.rosterless_user.id, ctx.user.id)
      refute Roster.get_pair(ctx.user.id, ctx.rosterless_user.id)
    end
  end

  describe "put/6 when there is no existing entry for the contact" do
    setup do
      contact = Factory.insert(:user)
      {:ok, contact: contact}
    end

    test "should insert a new contact", ctx do
      name = Name.first_name()
      groups = take_random(ctx.groups)

      assert {:ok, %RosterItem{}} =
               Roster.put(%{
                 user_id: ctx.user.id,
                 contact_id: ctx.contact.id,
                 name: name,
                 groups: groups,
                 ask: :out,
                 subscription: :both
               })

      item = Roster.get(ctx.user.id, ctx.contact.id)
      assert item.contact == ctx.contact
      assert item.name == name
      assert item.ask == :out
      assert item.subscription == :both
      assert length(item.groups) == length(groups)
    end

    test "should not fail with an empty name", ctx do
      groups = take_random(ctx.groups)

      assert {:ok, %RosterItem{}} =
               Roster.put(%{
                 user_id: ctx.user.id,
                 contact_id: ctx.contact.id,
                 name: "",
                 groups: groups,
                 ask: :out,
                 subscription: :both
               })
    end

    test "should return an error for an invalid user id", ctx do
      assert {:error, _} =
               Roster.put(%{
                 user_id: ID.new(),
                 contact_id: ctx.contact.id,
                 name: "",
                 groups: [],
                 ask: :out,
                 subscription: :both
               })
    end

    test "should return an error for an invalid contact id", ctx do
      assert {:error, _} =
               Roster.put(%{
                 user_id: ctx.user.id,
                 contact_id: ID.new(),
                 name: "",
                 groups: [],
                 ask: :out,
                 subscription: :both
               })
    end
  end

  describe "put/6 when there is an existing entry for the contact" do
    test "should update the existing contact", ctx do
      new_name = Name.first_name()
      new_groups = take_random(ctx.groups)

      assert {:ok, %RosterItem{}} =
               Roster.put(%{
                 user_id: ctx.user.id,
                 contact_id: ctx.contact.id,
                 name: new_name,
                 groups: new_groups,
                 ask: :out,
                 subscription: :both
               })

      item = Roster.get(ctx.user.id, ctx.contact.id)
      assert item.contact == ctx.contact
      assert item.name == new_name
      assert item.ask == :out
      assert item.subscription == :both
      assert item.groups == new_groups
    end
  end

  describe "version/1" do
    test "should return the version for the roster", ctx do
      assert is_binary(Roster.version(ctx.user.id))
    end

    test "should return 0-0 for a user with no roster items", ctx do
      assert Roster.version(ctx.rosterless_user.id) == "0-0"
    end

    test "should return 0-0 for a non-existant user" do
      assert Roster.version(ID.new()) == "0-0"
    end

    test "should change when the roster is written to", ctx do
      initial = Roster.version(ctx.user.id)

      Roster.put(%{
        user_id: ctx.user.id,
        contact_id: ctx.contact.id,
        name: Name.first_name(),
        groups: [],
        ask: :out,
        subscription: :both
      })

      refute Roster.version(ctx.user.id) == initial
    end
  end

  describe "delete/1" do
    test "should remove all contacts from the user", ctx do
      assert Roster.delete(ctx.user.id) == :ok
      refute Roster.get(ctx.user.id, ctx.contact.id)
      assert Roster.get(ctx.user.id) == []
    end

    test "should change the roster version", ctx do
      initial = Roster.version(ctx.user.id)
      assert Roster.delete(ctx.user.id) == :ok
      refute Roster.version(ctx.user.id) == initial
    end
  end

  describe "delete/2" do
    test "should remove the contact from the user's roster", ctx do
      assert Roster.delete(ctx.user.id, ctx.contact.id) == :ok
      refute Roster.get(ctx.user.id, ctx.contact.id)
      assert length(Roster.get(ctx.user.id)) == 6
    end

    test "should change the roster version", ctx do
      initial = Roster.version(ctx.user.id)
      assert Roster.delete(ctx.user.id, ctx.contact.id) == :ok
      refute Roster.version(ctx.user.id) == initial
    end
  end

  describe "find_users_with_contact/1" do
    test "should return the count of users with a given contact", ctx do
      assert Enum.sort(Roster.find_users_with_contact(ctx.user.id)) ==
               ctx.all_contacts

      assert Roster.find_users_with_contact(ctx.contact.id) == [ctx.user]
    end

    test "should return [] for a non-existant user" do
      assert Roster.find_users_with_contact(ID.new()) == []
    end

    test "should return [] for a user with no contacts" do
      user = Factory.insert(:user)
      assert Roster.find_users_with_contact(user.id) == []
    end
  end

  describe "has_contact/2" do
    test "should return true when the user has a the specified contact", ctx do
      assert Roster.has_contact(ctx.user.id, ctx.contact.id)
    end

    test "should return false when the user has the contact with non-none ask",
         ctx do
      Roster.put(default_item(ctx, ask: :out))

      refute Roster.has_contact(ctx.user.id, ctx.contact.id)
    end

    test "should return false for non-existant contacts", ctx do
      refute Roster.has_contact(ctx.user.id, ID.new())
    end
  end

  describe "friend?/2" do
    test "should return true when a user is subscribed", ctx do
      assert Roster.friend?(ctx.user.id, ctx.contact.id)
    end

    test "should return false if the user has blocked the contact", ctx do
      Block.block(ctx.user, ctx.contact)

      refute Roster.friend?(ctx.user.id, ctx.contact.id)
    end

    test "should return true if the contact has blocked the user", ctx do
      Block.block(ctx.contact, ctx.user)

      refute Roster.friend?(ctx.user.id, ctx.contact.id)
    end

    test "should return false if the contact does not have 'both' subscription",
         ctx do
      Roster.put(default_item(ctx, subscription: :from))

      refute Roster.friend?(ctx.user.id, ctx.contact.id)
    end

    test "should return false for non-existant contacts", ctx do
      refute Roster.friend?(ctx.user.id, ID.new())
      refute Roster.friend?(ctx.user.id, ctx.rosterless_user.id)
    end
  end

  describe "follower?/2" do
    test "should return true when a user is subscribed", ctx do
      assert Roster.follower?(ctx.user.id, ctx.contact.id)
    end

    test "should return false if the user has blocked the contact", ctx do
      Block.block(ctx.user, ctx.contact)

      refute Roster.follower?(ctx.user.id, ctx.contact.id)
    end

    test "should return false if the user is blocked by the contact", ctx do
      Block.block(ctx.contact, ctx.user)

      refute Roster.follower?(ctx.user.id, ctx.contact.id)
    end

    test "should return true if the user has 'to' subscription", ctx do
      Roster.put(default_item(ctx, subscription: :to))

      assert Roster.follower?(ctx.user.id, ctx.contact.id)
    end

    test "should return false if the user does not have 'both' or 'to' subscription",
         ctx do
      Roster.put(default_item(ctx, subscription: :from))

      refute Roster.follower?(ctx.user.id, ctx.contact.id)
    end

    test "should return false for non-existant contacts", ctx do
      refute Roster.follower?(ctx.user.id, ID.new())
      refute Roster.follower?(ctx.user.id, ctx.rosterless_user.id)
    end
  end

  describe "followers/1" do
    setup ctx do
      blocked_follower = Factory.insert(:user)
      RosterHelper.follow(blocked_follower, ctx.user)
      Block.block(ctx.user, blocked_follower)
      :ok
    end

    test "should return the full list of followers", ctx do
      assert Enum.sort(Roster.followers(ctx.user.id)) == ctx.visible_contacts
    end

    test "should optionally exclude system useres", ctx do
      assert Enum.sort(Roster.followers(ctx.user.id, false)) ==
               ctx.visible_contacts -- [ctx.system_user]
    end

    test "should not return users who aren't followers", ctx do
      Roster.put(default_item(ctx, subscription: :to))

      assert Enum.sort(Roster.followers(ctx.user.id)) ==
               ctx.visible_contacts -- [ctx.contact]
    end

    test "should return an empty list for non-users" do
      assert Roster.followers(ID.new()) == []
    end

    test "should return an empty list for users with no contacts", ctx do
      assert Roster.followers(ctx.rosterless_user.id) == []
    end
  end

  describe "followees/1" do
    setup do
      following_none = Factory.insert(:user)
      following_one = Factory.insert(:user)
      following_two = Factory.insert(:user)
      insert_follower_pair(following_two, following_one)
      insert_follower_pair(following_two, following_none)
      insert_follower_pair(following_one, following_none)

      {:ok,
       following_none: following_none,
       following_one: following_one,
       following_two: following_two,
       following_list: Enum.sort([following_none, following_one])}
    end

    test "should return the full list of users being followed", ctx do
      assert Roster.followees(ctx.following_none.id) == []
      assert Roster.followees(ctx.following_one.id) == [ctx.following_none]

      assert Enum.sort(Roster.followees(ctx.following_two.id)) ==
               ctx.following_list
    end

    test "should optionally not include system users", ctx do
      followees = Roster.followees(ctx.user.id, false)
      refute Enum.member?(followees, ctx.system_user)
    end

    test "should optionally include system users", ctx do
      followees = Roster.followees(ctx.user.id, true)
      assert Enum.member?(followees, ctx.system_user)
    end

    test "should return an empty list for non-users" do
      assert Roster.followees(ID.new()) == []
    end
  end

  describe "friends/1" do
    setup ctx do
      blocked_friend = Factory.insert(:user)
      RosterHelper.make_friends(blocked_friend, ctx.user)
      Block.block(ctx.user, blocked_friend)
      :ok
    end

    test "should return the full list of friends", ctx do
      assert Enum.sort(Roster.friends(ctx.user.id)) == ctx.visible_contacts
    end

    test "should optinally exclude system users", ctx do
      friends = Roster.friends(ctx.user.id, false)
      assert Enum.sort(friends) == ctx.visible_contacts -- [ctx.system_user]
    end

    test "should not return users who aren't friends", ctx do
      Roster.put(default_item(ctx, subscription: :from))

      friends = Roster.friends(ctx.user.id)
      assert Enum.sort(friends) == ctx.visible_contacts -- [ctx.contact]
    end

    test "should return an empty list for non-users" do
      assert Roster.friends(ID.new()) == []
    end

    test "should return an empty list for users with no contacts", ctx do
      assert Roster.friends(ctx.rosterless_user.id) == []
    end
  end

  describe "followers_query/2" do
    test "should return all followers", ctx do
      query = Roster.followers_query(ctx.followee.id, ctx.user.id)

      assert Repo.all(query) == [ctx.follower]
    end

    test "should exclude system users when set to do so", ctx do
      query = Roster.followers_query(ctx.user.id, ctx.user.id, false)

      assert Enum.sort(Repo.all(query)) ==
               ctx.visible_contacts -- [ctx.system_user]
    end

    test "should not return entries blocked by the requester", ctx do
      query = Roster.followers_query(ctx.followee.id, ctx.blocked_viewer.id)

      assert Repo.all(query) == []
    end
  end

  describe "followees_query/2" do
    test "should return all followees", ctx do
      query = Roster.followees_query(ctx.follower.id, ctx.user.id)

      assert Repo.all(query) == [ctx.followee]
    end

    test "should exclude system users when set to do so", ctx do
      query = Roster.followees_query(ctx.user.id, ctx.user.id, false)

      assert Enum.sort(Repo.all(query)) ==
               ctx.visible_contacts -- [ctx.system_user]
    end

    test "should not return entries blocked by the requester", ctx do
      query = Roster.followees_query(ctx.follower.id, ctx.blocked_viewer.id)

      assert Repo.all(query) == []
    end
  end

  describe "friends_query/2" do
    setup ctx do
      blocked_friend = Factory.insert(:user, %{first_name: "BLOCKYMCBLOCK"})
      insert_friend_pair(ctx.user, blocked_friend, [Lorem.word()])
      Block.block(blocked_friend, ctx.blocked_viewer)
      {:ok, blocked_friend: blocked_friend}
    end

    test "should return all friends", ctx do
      query = Roster.friends_query(ctx.user.id, ctx.follower.id)

      assert Enum.sort(Repo.all(query)) ==
               Enum.sort([ctx.blocked_friend | ctx.visible_contacts])
    end

    test "should exclude system users when set to do so", ctx do
      query = Roster.friends_query(ctx.user.id, ctx.user.id, false)

      assert Enum.sort(Repo.all(query)) ==
               Enum.sort([ctx.blocked_friend | ctx.visible_contacts]) --
                 [ctx.system_user]
    end

    test "should not return entries blocked by the requester", ctx do
      query = Roster.friends_query(ctx.user.id, ctx.blocked_viewer.id)

      assert Enum.sort(Repo.all(query)) == ctx.visible_contacts
    end
  end

  describe "relationship/2" do
    test "should return :self when both user IDs are equal", ctx do
      assert Roster.relationship(ctx.user.id, ctx.user.id) == :self
    end

    test "should return :friend where the two users are friends", ctx do
      assert Roster.relationship(ctx.user.id, ctx.contact.id) == :friend
      assert Roster.relationship(ctx.contact.id, ctx.user.id) == :friend
    end

    test "should return :follower where user a is following user b", ctx do
      assert Roster.relationship(ctx.follower.id, ctx.followee.id) == :follower
    end

    test "should return :followee where user b is following user a", ctx do
      assert Roster.relationship(ctx.followee.id, ctx.follower.id) == :followee
    end

    test "should return :none if the users have no relationship", ctx do
      assert Roster.relationship(ctx.user.id, ctx.rosterless_user.id) == :none
      assert Roster.relationship(ctx.rosterless_user.id, ctx.user.id) == :none
    end
  end

  describe "bump_all_versions/2" do
    test "should change the version for all roster entries with the contact",
         ctx do
      initial = Roster.version(ctx.user.id)

      assert Roster.bump_all_versions(ctx.contact.id) == :ok
      refute Roster.version(ctx.user.id) == initial
    end

    test "should not change the data", ctx do
      assert Roster.bump_all_versions(ctx.contact.id) == :ok

      result = Roster.get(ctx.user.id, ctx.contact.id)

      assert Map.get(result, :contact) == ctx.contact
    end
  end

  describe "relationship management functions" do
    setup do
      user2 = Factory.insert(:user)
      {:ok, user2: user2}
    end

    test "befriend/2 when there is no existing relationship", ctx do
      assert :ok = Roster.befriend(ctx.user.id, ctx.user2.id)
      assert Roster.friend?(ctx.user.id, ctx.user2.id)
    end

    test "befriend/2 when there is an existing relationship", ctx do
      name = Name.first_name()
      name2 = Name.first_name()

      Factory.insert(
        :roster_item,
        name: name,
        user_id: ctx.user.id,
        contact_id: ctx.user2.id,
        subscription: :from,
        name: name
      )

      Factory.insert(
        :roster_item,
        user_id: ctx.user2.id,
        contact_id: ctx.user.id,
        subscription: :to,
        name: name2
      )

      assert :ok = Roster.befriend(ctx.user.id, ctx.user2.id)
      assert Roster.friend?(ctx.user.id, ctx.user2.id)
      assert Roster.get(ctx.user.id, ctx.user2.id).name == name
      assert Roster.get(ctx.user2.id, ctx.user.id).name == name2
    end

    test "follow/2 when there is no existing relationship", ctx do
      assert :ok = Roster.follow(ctx.user.id, ctx.user2.id)

      assert Roster.follower?(ctx.user.id, ctx.user2.id)
      refute Roster.friend?(ctx.user.id, ctx.user2.id)
    end

    test "follow/2 when there is an existing relationship", ctx do
      name = Name.first_name()
      name2 = Name.first_name()

      Factory.insert(
        :roster_item,
        name: name,
        user_id: ctx.user.id,
        contact_id: ctx.user2.id,
        subscription: :both,
        name: name
      )

      Factory.insert(
        :roster_item,
        user_id: ctx.user2.id,
        contact_id: ctx.user.id,
        subscription: :both,
        name: name2
      )

      assert :ok = Roster.follow(ctx.user.id, ctx.user2.id)
      assert Roster.follower?(ctx.user.id, ctx.user2.id)
      assert Roster.get(ctx.user.id, ctx.user2.id).name == name
      assert Roster.get(ctx.user2.id, ctx.user.id).name == name2
    end

    test "unfriend/2 when users are friends", ctx do
      Roster.befriend(ctx.user.id, ctx.user2.id)

      assert :ok = Roster.unfriend(ctx.user.id, ctx.user2.id)
      assert Roster.relationship(ctx.user.id, ctx.user2.id) == :none
    end

    test "unfriend/2 when a is following b", ctx do
      Roster.follow(ctx.user.id, ctx.user2.id)

      assert :ok = Roster.unfriend(ctx.user.id, ctx.user2.id)
      assert Roster.relationship(ctx.user.id, ctx.user2.id) == :none
    end

    test "unfriend/2 when b is following a", ctx do
      Roster.follow(ctx.user.id, ctx.user2.id)

      assert :ok = Roster.unfriend(ctx.user.id, ctx.user2.id)
      assert Roster.relationship(ctx.user.id, ctx.user2.id) == :none
    end

    test "unfriend/2 when there is no existing relationship", ctx do
      assert :ok = Roster.unfriend(ctx.user.id, ctx.user2.id)
      assert Roster.relationship(ctx.user.id, ctx.user2.id) == :none
    end
  end

  defp insert_friend_pair(user, contact, groups) do
    a =
      Factory.insert(
        :roster_item,
        user_id: user.id,
        contact_id: contact.id,
        groups: take_random(groups)
      )

    b =
      Factory.insert(
        :roster_item,
        user_id: contact.id,
        contact_id: user.id,
        groups: take_random(groups)
      )

    {a, b}
  end

  defp insert_follower_pair(follower, followee) do
    Factory.insert(
      :roster_item,
      subscription: :from,
      user_id: followee.id,
      contact_id: follower.id
    )

    Factory.insert(
      :roster_item,
      subscription: :to,
      user_id: follower.id,
      contact_id: followee.id
    )
  end

  defp take_random(list) do
    Enum.take_random(list, :rand.uniform(length(list)))
  end

  defp default_item(ctx, replace) do
    r = %{
      user_id: ctx.user.id,
      contact_id: ctx.contact.id,
      name: Name.first_name(),
      groups: [],
      ask: :none,
      subscription: :both
    }

    Map.merge(r, Map.new(replace))
  end
end
