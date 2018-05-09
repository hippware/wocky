# credo:disable-for-this-file Credo.Check.Refactor.PipeChainStart
defmodule Wocky.RosterSpec do
  use ESpec, async: true

  alias Faker.Lorem
  alias Faker.Name
  alias Wocky.Block
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.Roster
  alias Wocky.Roster.Item, as: RosterItem
  alias Wocky.User

  before do
    # A user with 5 contacts in a randomised subset of 5 groups
    user = Factory.insert(:user)

    contacts =
      for _ <- 1..5 do
        Factory.insert(:user)
      end

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
    it "should return all roster items for a user" do
      Roster.get(shared.user.id)
      |> Enum.map(&Map.get(&1, :contact))
      |> Enum.sort()
      |> should(eq shared.all_contacts)
    end

    it "should return an empty list for a user with no roster items" do
      Roster.get(shared.rosterless_user.id) |> should(eq [])
    end

    it "should return an empty list for a non-existant user" do
      Roster.get(ID.new()) |> should(eq [])
    end
  end

  describe "get/2" do
    it "should return the roster item for the specified contact" do
      Enum.map(shared.all_contacts, fn c ->
        Roster.get(shared.user.id, c.id)
        |> Map.get(:contact)
        |> should(eq c)
      end)
    end
  end

  describe "get_pair/2" do
    it "should return the pair of roster items with the first one first" do
      {a, b} = shared.roster_pair

      Roster.get_pair(shared.user.id, shared.contact.id)
      |> should(eq {a, b})

      Roster.get_pair(shared.contact.id, shared.user.id)
      |> should(eq {b, a})
    end

    it "should return nil where no relationship exists" do
      Roster.get_pair(shared.rosterless_user.id, shared.user.id)
      |> should(be_nil())

      Roster.get_pair(shared.user.id, shared.rosterless_user.id)
      |> should(be_nil())
    end
  end

  describe "put/6" do
    context "when there is no existing entry for the contact" do
      before do
        contact = Factory.insert(:user, %{server: shared.server})
        {:ok, contact: contact}
      end

      it "should insert a new contact" do
        name = Name.first_name()
        groups = take_random(shared.groups)

        put_result =
          Roster.put(%{
            user_id: shared.user.id,
            contact_id: shared.contact.id,
            name: name,
            groups: groups,
            ask: :out,
            subscription: :both
          })

        put_result |> should(be_ok_result())
        put_result |> Kernel.elem(1) |> should(be_struct RosterItem)

        item = Roster.get(shared.user.id, shared.contact.id)
        item.contact |> should(eq shared.contact)
        item.name |> should(eq name)
        item.ask |> should(eq :out)
        item.subscription |> should(eq :both)
        item.groups |> should(have_count length(groups))
      end

      it "should not fail with an empty name" do
        groups = take_random(shared.groups)

        put_result =
          Roster.put(%{
            user_id: shared.user.id,
            contact_id: shared.contact.id,
            name: "",
            groups: groups,
            ask: :out,
            subscription: :both
          })

        put_result |> should(be_ok_result())
      end

      it "should return an error for an invalid user id" do
        put_result =
          Roster.put(%{
            user_id: ID.new(),
            contact_id: shared.contact.id,
            name: "",
            groups: [],
            ask: :out,
            subscription: :both
          })

        put_result |> should(be_error_result())
      end

      it "should return an error for an invalid contact id" do
        put_result =
          Roster.put(%{
            user_id: shared.user.id,
            contact_id: ID.new(),
            name: "",
            groups: [],
            ask: :out,
            subscription: :both
          })

        put_result |> should(be_error_result())
      end
    end

    context "when there is an existing entry for the contact" do
      it "should update the existing contact" do
        new_name = Name.first_name()
        new_groups = take_random(shared.groups)

        put_result =
          Roster.put(%{
            user_id: shared.user.id,
            contact_id: shared.contact.id,
            name: new_name,
            groups: new_groups,
            ask: :out,
            subscription: :both
          })

        put_result |> should(be_ok_result())
        put_result |> Kernel.elem(1) |> should(be_struct RosterItem)

        item = Roster.get(shared.user.id, shared.contact.id)
        item.contact |> should(eq shared.contact)
        item.name |> should(eq new_name)
        item.ask |> should(eq :out)
        item.subscription |> should(eq :both)
        item.groups |> should(eq new_groups)
      end
    end
  end

  describe "version/1" do
    it "should return the version for the roster" do
      Roster.version(shared.user.id)
      |> should(be_binary())
    end

    it "should return 0-0 for a user with no roster items" do
      Roster.version(shared.rosterless_user.id) |> should(eq "0-0")
    end

    it "should return 0-0 for a non-existant user" do
      Roster.version(ID.new()) |> should(eq "0-0")
    end

    it "should change when the roster is written to" do
      initial = Roster.version(shared.user.id)

      Roster.put(%{
        user_id: shared.user.id,
        contact_id: shared.contact.id,
        name: Name.first_name(),
        groups: [],
        ask: :out,
        subscription: :both
      })

      Roster.version(shared.user.id) |> should(be :!=, initial)
    end
  end

  describe "delete/1" do
    it "should remove all contacts from the user" do
      Roster.delete(shared.user.id) |> should(eq :ok)

      Roster.get(shared.user.id, shared.contact.id)
      |> should(eq nil)

      Roster.get(shared.user.id) |> should(have_length 0)
    end

    it "should change the roster version" do
      initial = Roster.version(shared.user.id)
      Roster.delete(shared.user.id) |> should(eq :ok)
      Roster.version(shared.user.id) |> should(be :!=, initial)
    end
  end

  describe "delete/2" do
    it "should remove the contact from the user's roster" do
      Roster.delete(shared.user.id, shared.contact.id) |> should(eq :ok)

      Roster.get(shared.user.id, shared.contact.id)
      |> should(eq nil)

      Roster.get(shared.user.id) |> should(have_length 6)
    end

    it "should change the roster version" do
      initial = Roster.version(shared.user.id)
      Roster.delete(shared.user.id, shared.contact.id) |> should(eq :ok)
      Roster.version(shared.user.id) |> should(be :!=, initial)
    end
  end

  describe "find_users_with_contact/1" do
    it "should return the count of users with a given contact" do
      Roster.find_users_with_contact(shared.user.id)
      |> Enum.sort()
      |> should(eq shared.all_contacts)

      Roster.find_users_with_contact(shared.contact.id)
      |> should(eq [shared.user])
    end

    it "should return [] for a non-existant user" do
      Roster.find_users_with_contact(ID.new()) |> should(eq [])
    end

    it "should return [] for a user with no contacts" do
      user = Factory.insert(:user, %{server: shared.server})
      Roster.find_users_with_contact(user.id) |> should(eq [])
    end
  end

  describe "has_contact/2" do
    it "should return true when the user has a the specified contact" do
      Roster.has_contact(shared.user.id, shared.contact.id)
      |> should(be_true())
    end

    it "should return false when the user has the contact with non-none ask" do
      Roster.put(default_item(shared, ask: :out))

      Roster.has_contact(shared.user.id, shared.contact.id)
      |> should(be_false())
    end

    it "should return false for non-existant contacts" do
      Roster.has_contact(shared.user.id, ID.new())
      |> should(be_false())
    end
  end

  describe "is_friend/2" do
    it "should return true when a user is subscribed" do
      Roster.is_friend(shared.user.id, shared.contact.id)
      |> should(be_true())
    end

    it "should return false if the user has blocked the contact" do
      Block.block(shared.user, shared.contact)

      Roster.is_friend(shared.user.id, shared.contact.id)
      |> should(be_false())
    end

    it "should return true if the contact has blocked the user" do
      Block.block(shared.contact, shared.user)

      Roster.is_friend(shared.user.id, shared.contact.id)
      |> should(be_false())
    end

    it "should return false if the contact does not have 'both' subscription" do
      Roster.put(default_item(shared, subscription: :from))

      Roster.is_friend(shared.user.id, shared.contact.id)
      |> should(be_false())
    end

    it "should return false for non-existant contacts" do
      Roster.is_friend(shared.user.id, ID.new())
      |> should(be_false())

      Roster.is_friend(shared.user.id, shared.rosterless_user.id)
      |> should(be_false())
    end
  end

  describe "is_follower/2" do
    it "should return true when a user is subscribed" do
      Roster.is_follower(shared.user.id, shared.contact.id)
      |> should(be_true())
    end

    it "should return false if the user has blocked the contact" do
      Block.block(shared.user, shared.contact)

      Roster.is_follower(shared.user.id, shared.contact.id)
      |> should(be_false())
    end

    it "should return false if the user is blocked by the contact" do
      Block.block(shared.contact, shared.user)

      Roster.is_follower(shared.user.id, shared.contact.id)
      |> should(be_false())
    end

    it "should return true if the user has 'to' subscription" do
      Roster.put(default_item(shared, subscription: :to))

      Roster.is_follower(shared.user.id, shared.contact.id)
      |> should(be_true())
    end

    it "should return false if the user does not have 'both' or 'to' subscription" do
      Roster.put(default_item(shared, subscription: :from))

      Roster.is_follower(shared.user.id, shared.contact.id)
      |> should(be_false())
    end

    it "should return false for non-existant contacts" do
      Roster.is_follower(shared.user.id, ID.new())
      |> should(be_false())

      Roster.is_follower(shared.user.id, shared.rosterless_user.id)
      |> should(be_false())
    end
  end

  describe "followers/1" do
    before do
      blocked_follower = Factory.insert(:user, %{server: shared.server})
      RosterHelper.follow(blocked_follower, shared.user)
      Block.block(shared.user, blocked_follower)
      :ok
    end

    it "should return the full list of followers" do
      Roster.followers(shared.user.id)
      |> Enum.sort()
      |> should(eq shared.visible_contacts)
    end

    it "should optionally exclude system useres" do
      Roster.followers(shared.user.id, false)
      |> Enum.sort()
      |> should(eq shared.visible_contacts -- [shared.system_user])
    end

    it "should not return users who aren't followers" do
      Roster.put(default_item(shared, subscription: :to))

      Roster.followers(shared.user.id)
      |> Enum.sort()
      |> should(eq shared.visible_contacts -- [shared.contact])
    end

    it "should return an empty list for non-users" do
      Roster.followers(ID.new()) |> should(eq [])
    end

    it "should return an empty list for users with no contacts" do
      Roster.followers(shared.rosterless_user.id) |> should(eq [])
    end
  end

  describe "followees/1" do
    before do
      following_none = Factory.insert(:user, %{server: shared.server})
      following_one = Factory.insert(:user, %{server: shared.server})
      following_two = Factory.insert(:user, %{server: shared.server})
      insert_follower_pair(following_two, following_one)
      insert_follower_pair(following_two, following_none)
      insert_follower_pair(following_one, following_none)

      {:ok,
       following_none: following_none,
       following_one: following_one,
       following_two: following_two,
       following_list: Enum.sort([following_none, following_one])}
    end

    it "should return the full list of users being followed" do
      shared.following_none.id
      |> Roster.followees()
      |> should(eq [])

      shared.following_one.id
      |> Roster.followees()
      |> should(eq [shared.following_none])

      shared.following_two.id
      |> Roster.followees()
      |> Enum.sort()
      |> should(eq shared.following_list)
    end

    it "should optionally not include system users" do
      Roster.followees(shared.user.id, false)
      |> should_not(have shared.system_user)
    end

    it "should optionally include system users" do
      Roster.followees(shared.user.id, true)
      |> should(have shared.system_user)
    end

    it "should return an empty list for non-users" do
      Roster.followees(ID.new()) |> should(eq [])
    end
  end

  describe "friends/1" do
    before do
      blocked_friend = Factory.insert(:user, %{server: shared.server})
      RosterHelper.make_friends(blocked_friend, shared.user)
      Block.block(shared.user, blocked_friend)
      :ok
    end

    it "should return the full list of friends" do
      Roster.friends(shared.user.id)
      |> Enum.sort()
      |> should(eq shared.visible_contacts)
    end

    it "should optinally exclude system users" do
      Roster.friends(shared.user.id, false)
      |> Enum.sort()
      |> should(eq shared.visible_contacts -- [shared.system_user])
    end

    it "should not return users who aren't friends" do
      Roster.put(default_item(shared, subscription: :from))

      Roster.friends(shared.user.id)
      |> Enum.sort()
      |> should(eq shared.visible_contacts -- [shared.contact])
    end

    it "should return an empty list for non-users" do
      Roster.friends(ID.new()) |> should(eq [])
    end

    it "should return an empty list for users with no contacts" do
      Roster.friends(shared.rosterless_user.id) |> should(eq [])
    end
  end

  describe "followers_query/2" do
    it "should return all followers" do
      Roster.followers_query(shared.followee.id, shared.user.id)
      |> Repo.all()
      |> should(eq [shared.follower])
    end

    it "should exclude system users when set to do so" do
      Roster.followers_query(shared.user.id, shared.user.id, false)
      |> Repo.all()
      |> Enum.sort()
      |> should(eq shared.visible_contacts -- [shared.system_user])
    end

    it "should not return entries blocked by the requester" do
      Roster.followers_query(shared.followee.id, shared.blocked_viewer.id)
      |> Repo.all()
      |> should(eq [])
    end
  end

  describe "followees_query/2" do
    it "should return all followees" do
      Roster.followees_query(shared.follower.id, shared.user.id)
      |> Repo.all()
      |> should(eq [shared.followee])
    end

    it "should exclude system users when set to do so" do
      Roster.followees_query(shared.user.id, shared.user.id, false)
      |> Repo.all()
      |> Enum.sort()
      |> should(eq shared.visible_contacts -- [shared.system_user])
    end

    it "should not return entries blocked by the requester" do
      Roster.followees_query(shared.follower.id, shared.blocked_viewer.id)
      |> Repo.all()
      |> should(eq [])
    end
  end

  describe "friends_query/2" do
    before do
      blocked_friend = Factory.insert(:user, %{first_name: "BLOCKYMCBLOCK"})
      insert_friend_pair(shared.user, blocked_friend, [Lorem.word()])
      Block.block(blocked_friend, shared.blocked_viewer)
      {:ok, blocked_friend: blocked_friend}
    end

    it "should return all friends" do
      Roster.friends_query(shared.user.id, shared.follower.id)
      |> Repo.all()
      |> Enum.sort()
      |> should(
        eq Enum.sort([
             shared.blocked_friend
             | shared.visible_contacts
           ])
      )
    end

    it "should exclude system users when set to do so" do
      Roster.friends_query(shared.user.id, shared.user.id, false)
      |> Repo.all()
      |> Enum.sort()
      |> should(
        eq Enum.sort([shared.blocked_friend | shared.visible_contacts]) --
             [shared.system_user]
      )
    end

    it "should not return entries blocked by the requester" do
      Roster.friends_query(shared.user.id, shared.blocked_viewer.id)
      |> Repo.all()
      |> Enum.sort()
      |> should(eq shared.visible_contacts)
    end
  end

  describe "relationship/2" do
    it "should return :self when both user IDs are equal" do
      Roster.relationship(shared.user.id, shared.user.id)
      |> should(eq :self)
    end

    it "should return :friend where the two users are friends" do
      Roster.relationship(shared.user.id, shared.contact.id)
      |> should(eq :friend)

      Roster.relationship(shared.contact.id, shared.user.id)
      |> should(eq :friend)
    end

    it "should return :follower where user a is following user b" do
      Roster.relationship(shared.follower.id, shared.followee.id)
      |> should(eq :follower)
    end

    it "should return :followee where user b is following user a" do
      Roster.relationship(shared.followee.id, shared.follower.id)
      |> should(eq :followee)
    end

    it "should return :none if the users have no relationship" do
      Roster.relationship(shared.user.id, shared.rosterless_user.id)
      |> should(eq :none)

      Roster.relationship(shared.rosterless_user.id, shared.user.id)
      |> should(eq :none)
    end
  end

  describe "bump_all_versions/2" do
    it "should change the version for all roster entries with the contact" do
      initial = Roster.version(shared.user.id)

      Roster.bump_all_versions(shared.contact.id)
      |> should(eq :ok)

      Roster.version(shared.user.id) |> should(be :!=, initial)
    end

    it "should not change the data" do
      Roster.bump_all_versions(shared.contact.id)
      |> should(eq :ok)

      Roster.get(shared.user.id, shared.contact.id)
      |> Map.get(:contact)
      |> should(eq shared.contact)
    end
  end

  describe "relationship management functions" do
    before do
      user2 = Factory.insert(:user)
      {:ok, user2: user2}
    end

    describe "befriend/2" do
      context "when there is no existing relationship" do
        before do
          result = Roster.befriend(shared.user.id, shared.user2.id)
          {:ok, result: result}
        end

        it "should return ok" do
          shared.result |> should(eq :ok)
        end

        it "should make the users friends" do
          Roster.is_friend(shared.user.id, shared.user2.id)
          |> should(be_true())
        end
      end

      context "when there is an existing relationship" do
        before do
          name = Name.first_name()
          name2 = Name.first_name()

          Factory.insert(
            :roster_item,
            name: name,
            user_id: shared.user.id,
            contact_id: shared.user2.id,
            subscription: :from,
            name: name
          )

          Factory.insert(
            :roster_item,
            user_id: shared.user2.id,
            contact_id: shared.user.id,
            subscription: :to,
            name: name2
          )

          result = Roster.befriend(shared.user.id, shared.user2.id)
          {:ok, name: name, name2: name2, result: result}
        end

        it "should return ok" do
          shared.result |> should(eq :ok)
        end

        it "should make the users friends" do
          Roster.is_friend(shared.user.id, shared.user2.id)
          |> should(be_true())
        end

        it "should not remove the existing name data" do
          Roster.get(shared.user.id, shared.user2.id).name
          |> should(eq shared.name)

          Roster.get(shared.user2.id, shared.user.id).name
          |> should(eq shared.name2)
        end
      end
    end

    describe "follow/2" do
      context "when there is no existing relationship" do
        before do
          result = Roster.follow(shared.user.id, shared.user2.id)
          {:ok, result: result}
        end

        it "should return ok" do
          shared.result |> should(eq :ok)
        end

        it "should make user1 follow user2" do
          Roster.is_follower(shared.user.id, shared.user2.id)
          |> should(be_true())
        end

        it "should not make the users friends" do
          Roster.is_friend(shared.user.id, shared.user2.id)
          |> should(be_false())
        end
      end

      context "when there is an existing relationship" do
        before do
          name = Name.first_name()
          name2 = Name.first_name()

          Factory.insert(
            :roster_item,
            name: name,
            user_id: shared.user.id,
            contact_id: shared.user2.id,
            subscription: :both,
            name: name
          )

          Factory.insert(
            :roster_item,
            user_id: shared.user2.id,
            contact_id: shared.user.id,
            subscription: :both,
            name: name2
          )

          result = Roster.follow(shared.user.id, shared.user2.id)
          {:ok, name: name, name2: name2, result: result}
        end

        it "should return ok" do
          shared.result |> should(eq :ok)
        end

        it "should make user1 follow user 2" do
          Roster.is_follower(shared.user.id, shared.user2.id)
          |> should(be_true())
        end

        it "should not remove the existing name data" do
          Roster.get(shared.user.id, shared.user2.id).name
          |> should(eq shared.name)

          Roster.get(shared.user2.id, shared.user.id).name
          |> should(eq shared.name2)
        end
      end
    end

    describe "unfriend/2" do
      context "when users are friends" do
        before do
          Roster.befriend(shared.user.id, shared.user2.id)
          result = Roster.unfriend(shared.user.id, shared.user2.id)
          {:ok, result: result}
        end

        it "should return ok" do
          shared.result |> should(eq :ok)
        end

        it "should make the users have no relationship" do
          Roster.relationship(shared.user.id, shared.user2.id)
          |> should(eq :none)
        end
      end

      context "when a is following b" do
        before do
          Roster.follow(shared.user.id, shared.user2.id)
          result = Roster.unfriend(shared.user.id, shared.user2.id)
          {:ok, result: result}
        end

        it "should return ok" do
          shared.result |> should(eq :ok)
        end

        it "should make the users have no relationship" do
          Roster.relationship(shared.user.id, shared.user2.id)
          |> should(eq :none)
        end
      end

      context "when b is following a" do
        before do
          Roster.follow(shared.user.id, shared.user2.id)
          result = Roster.unfriend(shared.user.id, shared.user2.id)
          {:ok, result: result}
        end

        it "should return ok" do
          shared.result |> should(eq :ok)
        end

        it "should make the users have no relationship" do
          Roster.relationship(shared.user.id, shared.user2.id)
          |> should(eq :none)
        end
      end

      context "when there is no existing relationship" do
        before do
          result = Roster.unfriend(shared.user.id, shared.user2.id)
          {:ok, result: result}
        end

        it "should return ok" do
          shared.result |> should(eq :ok)
        end

        it "should make the users have no relationship" do
          Roster.relationship(shared.user.id, shared.user2.id)
          |> should(eq :none)
        end
      end
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

  defp default_item(shared, replace) do
    r = %{
      user_id: shared.user.id,
      contact_id: shared.contact.id,
      name: Name.first_name(),
      groups: [],
      ask: :none,
      subscription: :both
    }

    Map.merge(r, Map.new(replace))
  end
end
