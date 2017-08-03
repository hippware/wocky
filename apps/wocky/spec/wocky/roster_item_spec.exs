defmodule Wocky.RosterItemSpec do
  use ESpec, async: true

  alias Faker.Lorem
  alias Faker.Name
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.RosterItem

  before do
    # A user with 5 contacts in a randomised subset of 5 groups
    user = Factory.insert(:user, %{server: shared.server})
    contacts = for _ <- 1..5 do
      Factory.insert(:user, %{server: shared.server})
    end
    groups = for _ <- 1..5, do: Lorem.word
    roster_pairs = Enum.map(contacts, &insert_roster_pair(user, &1, groups))

    rosterless_user = Factory.insert(:user, %{server: shared.server})

    follower = Factory.insert(:user, %{server: shared.server})
    followee = Factory.insert(:user, %{server: shared.server})
    insert_follower_pair(follower, followee)

    {:ok,
     user: user,
     contacts: Enum.sort(contacts),
     contact: hd(contacts),
     roster_pairs: roster_pairs,
     roster_pair: hd(roster_pairs),
     rosterless_user: rosterless_user,
     follower: follower,
     followee: followee,
     groups: groups}
  end

  describe "get/1" do
    it "should return all roster items for a user" do
      RosterItem.get(shared.user.id)
      |> Enum.map(&Map.get(&1, :contact))
      |> Enum.sort
      |> should(eq shared.contacts)
    end

    it "should return an empty list for a user with no roster items" do
      RosterItem.get(shared.rosterless_user.id) |> should(eq [])
    end

    it "should return an empty list for a non-existant user" do
      RosterItem.get(ID.new) |> should(eq [])
    end
  end

  describe "get/2" do
    it "should return the roster item for the specified contact" do
      Enum.map shared.contacts, fn(c) ->
        RosterItem.get(shared.user.id, c.id)
        |> Map.get(:contact)
        |> should(eq c)
      end
    end
  end

  describe "get_pair/2" do
    it "should return the pair of roster items with the first one first" do
      {a, b} = shared.roster_pair
      RosterItem.get_pair(shared.user.id, shared.contact.id)
      |> should(eq {a, b})

      RosterItem.get_pair(shared.contact.id, shared.user.id)
      |> should(eq {b, a})
    end

    it "should return nil where no relationship exists" do
      RosterItem.get_pair(shared.rosterless_user.id, shared.user.id)
      |> should(be_nil())

      RosterItem.get_pair(shared.user.id, shared.rosterless_user.id)
      |> should(be_nil())
    end
  end

  describe "put/6" do
    context "when there is no existing entry for the contact" do
      it "should insert a new contact" do
        contact = Factory.insert(:user, %{server: shared.server})
        name = Name.first_name
        groups = take_random(shared.groups)
        put_result = RosterItem.put(%{user_id: shared.user.id,
                                      contact_id: contact.id,
                                      name: name,
                                      groups: groups,
                                      ask: :out,
                                      subscription: :both})
        put_result |> should(be_ok_result())
        put_result |> Kernel.elem(1) |> should(be_struct RosterItem)

        item = RosterItem.get(shared.user.id, contact.id)
        item.contact |> should(eq contact)
        item.name |> should(eq name)
        item.ask |> should(eq :out)
        item.subscription |> should(eq :both)
        item.groups |> should(have_count length(groups))
      end

      it "should not fail with an empty name" do
        contact = Factory.insert(:user, %{server: shared.server})
        groups = take_random(shared.groups)
        put_result = RosterItem.put(%{user_id: shared.user.id,
                                      contact_id: contact.id,
                                      name: "",
                                      groups: groups,
                                      ask: :out,
                                      subscription: :both})
        put_result |> should(be_ok_result())
      end
    end

    context "when there is an existing entry for the contact" do
      it "should update the existing contact" do
        new_name = Name.first_name
        new_groups = take_random(shared.groups)
        put_result = RosterItem.put(%{user_id: shared.user.id,
                                      contact_id: shared.contact.id,
                                      name: new_name,
                                      groups: new_groups,
                                      ask: :out,
                                      subscription: :both})
        put_result |> should(be_ok_result())
        put_result |> Kernel.elem(1) |> should(be_struct RosterItem)

        item = RosterItem.get(shared.user.id, shared.contact.id)
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
      RosterItem.version(shared.user.id)
      |> should(be_binary())
    end

    it "should return 0-0 for a user with no roster items" do
      RosterItem.version(shared.rosterless_user.id) |> should(eq "0-0")
    end

    it "should return 0-0 for a non-existant user" do
      RosterItem.version(ID.new) |> should(eq "0-0")
    end

    it "should change when the roster is written to" do
      initial = RosterItem.version(shared.user.id)
      RosterItem.put(%{user_id: shared.user.id,
                       contact_id: shared.contact.id,
                       name: Name.first_name,
                       groups: [],
                       ask: :out,
                       subscription: :both})
      RosterItem.version(shared.user.id) |> should(be :!=, initial)
    end
  end

  describe "delete/1" do
    it "should remove all contacts from the user" do
      RosterItem.delete(shared.user.id) |> should(eq :ok)
      RosterItem.get(shared.user.id, shared.contact.id)
      |> should(eq nil)
      RosterItem.get(shared.user.id) |> should(have_length 0)
    end

    it "should change the roster version" do
      initial = RosterItem.version(shared.user.id)
      RosterItem.delete(shared.user.id) |> should(eq :ok)
      RosterItem.version(shared.user.id) |> should(be :!=, initial)
    end
  end

  describe "delete/2" do
    it "should remove the contact from the user's roster" do
      RosterItem.delete(shared.user.id, shared.contact.id) |> should(eq :ok)
      RosterItem.get(shared.user.id, shared.contact.id)
      |> should(eq nil)
      RosterItem.get(shared.user.id) |> should(have_length 4)
    end

    it "should change the roster version" do
      initial = RosterItem.version(shared.user.id)
      RosterItem.delete(shared.user.id, shared.contact.id) |> should(eq :ok)
      RosterItem.version(shared.user.id) |> should(be :!=, initial)
    end
  end

  describe "find_users_with_contact/1" do
    it "should return the count of users with a given contact" do
      RosterItem.find_users_with_contact(shared.user.id)
      |> Enum.sort
      |> should(eq shared.contacts)
      RosterItem.find_users_with_contact(shared.contact.id)
      |> should(eq [shared.user])
    end

    it "should return [] for a non-existant user" do
      RosterItem.find_users_with_contact(ID.new) |> should(eq [])
    end

    it "should return [] for a user with no contacts" do
      user = Factory.insert(:user, %{server: shared.server})
      RosterItem.find_users_with_contact(user.id) |> should(eq [])
    end
  end

  describe "has_contact/2" do
    it "should return true when the user has a the specified contact" do
      RosterItem.has_contact(shared.user.id, shared.contact.id)
      |> should(be_true())
    end

    it "should return false when the user has the contact with non-none ask" do
      RosterItem.put(default_item(shared, ask: :out))
      RosterItem.has_contact(shared.user.id, shared.contact.id)
      |> should(be_false())
    end

    it "should return false for non-existant contacts" do
      RosterItem.has_contact(shared.user.id, ID.new)
      |> should(be_false())
    end
  end

  describe "is_friend/2" do
    it "should return true when a user is subscribed" do
      RosterItem.is_friend(shared.user.id, shared.contact.id)
      |> should(be_true())
    end

    it "should return false if the contact is in the __blocked__ group" do
      RosterItem.put(default_item(shared, groups: ["__blocked__"]))
      RosterItem.is_friend(shared.user.id, shared.contact.id)
      |> should(be_false())
    end

    it "should return true if the user is in the contact's __blocked__ group" do
      RosterItem.put(default_item(shared, user_id: shared.contact.id,
                                          contact_id: shared.user.id,
                                          groups: ["__blocked__"]))
      RosterItem.is_friend(shared.user.id, shared.contact.id)
      |> should(be_true())
    end

    it "should return false if the contact does not have 'both' subscription" do
      RosterItem.put(default_item(shared, subscription: :from))
      RosterItem.is_friend(shared.user.id, shared.contact.id)
      |> should(be_false())
    end

    it "should return false for non-existant contacts" do
      RosterItem.is_friend(shared.user.id, ID.new)
      |> should(be_false())
      RosterItem.is_friend(shared.user.id, shared.rosterless_user.id)
      |> should(be_false())
    end
  end

  describe "is_unblocked_friend/2" do
    it "should return true when a user is subscribed" do
      RosterItem.is_unblocked_friend(shared.user.id, shared.contact.id)
      |> should(be_true())
    end

    it "should return false if the contact is in the __blocked__ group" do
      RosterItem.put(default_item(shared, groups: ["__blocked__"]))
      RosterItem.is_unblocked_friend(shared.user.id, shared.contact.id)
      |> should(be_false())
    end

    it "should return false if the user is in the contact's __blocked__ group" do
      RosterItem.put(default_item(shared, user_id: shared.contact.id,
                                          contact_id: shared.user.id,
                                          groups: ["__blocked__"]))
      RosterItem.is_unblocked_friend(shared.user.id, shared.contact.id)
      |> should(be_false())
    end

    it "should return false if the contact does not have 'both' subscription" do
      RosterItem.put(default_item(shared, subscription: :from))
      RosterItem.is_unblocked_friend(shared.user.id, shared.contact.id)
      |> should(be_false())
    end

    it "should return false for non-existant contacts" do
      RosterItem.is_unblocked_friend(shared.user.id, ID.new)
      |> should(be_false())
      RosterItem.is_unblocked_friend(shared.user.id, shared.rosterless_user.id)
      |> should(be_false())
    end
  end


  describe "is_follower/2" do
    it "should return true when a user is subscribed" do
      RosterItem.is_follower(shared.user.id, shared.contact.id)
      |> should(be_true())
    end

    it "should return false if the contact is in the __blocked__ group" do
      RosterItem.put(default_item(shared, groups: ["__blocked__"]))
      RosterItem.is_follower(shared.user.id, shared.contact.id)
      |> should(be_false())
    end

    it "should return true if the user is in the contact's __blocked__ group" do
      RosterItem.put(default_item(shared, user_id: shared.contact.id,
                                          contact_id: shared.user.id,
                                          subscription: :from,
                                          groups: ["__blocked__"]))
      RosterItem.is_follower(shared.user.id, shared.contact.id)
      |> should(be_true())
    end

    it "should return true if the user has 'to' subscription" do
      RosterItem.put(default_item(shared, subscription: :to))
      RosterItem.is_follower(shared.user.id, shared.contact.id)
      |> should(be_true())
    end

    it "should return false if the user does not have 'both' or 'to' subscription" do
      RosterItem.put(default_item(shared, subscription: :from))
      RosterItem.is_follower(shared.user.id, shared.contact.id)
      |> should(be_false())
    end

    it "should return false for non-existant contacts" do
      RosterItem.is_follower(shared.user.id, ID.new)
      |> should(be_false())
      RosterItem.is_follower(shared.user.id, shared.rosterless_user.id)
      |> should(be_false())
    end
  end

  describe "is_unblocked_follower/2" do
    it "should return true when a user is subscribed" do
      RosterItem.is_unblocked_follower(shared.user.id, shared.contact.id)
      |> should(be_true())
    end

    it "should return false if the contact is in the __blocked__ group" do
      RosterItem.put(default_item(shared, groups: ["__blocked__"]))
      RosterItem.is_unblocked_follower(shared.user.id, shared.contact.id)
      |> should(be_false())
    end

    it "should return true if the user is in the contact's __blocked__ group" do
      RosterItem.put(default_item(shared, user_id: shared.contact.id,
                                          contact_id: shared.user.id,
                                          subscription: :from,
                                          groups: ["__blocked__"]))
      RosterItem.is_unblocked_follower(shared.user.id, shared.contact.id)
      |> should(be_false())
    end


    it "should return true if the user has 'from' subscription" do
      RosterItem.put(default_item(shared, subscription: :to))
      RosterItem.is_unblocked_follower(shared.user.id, shared.contact.id)
      |> should(be_true())
    end

    it "should return false if the user does not have 'both' or 'to' subscription" do
      RosterItem.put(default_item(shared, subscription: :from))
      RosterItem.is_unblocked_follower(shared.user.id, shared.contact.id)
      |> should(be_false())
    end

    it "should return false for non-existant contacts" do
      RosterItem.is_unblocked_follower(shared.user.id, ID.new)
      |> should(be_false())
      RosterItem.is_unblocked_follower(shared.user.id, shared.rosterless_user.id)
      |> should(be_false())
    end
  end


  describe "followers/1" do
    before do
      blocked_follower = Factory.insert(:user, %{server: shared.server})
      Factory.insert(
        :roster_item,
        user_id: shared.user.id,
        contact_id: blocked_follower.id,
        subscription: :from,
        groups: [RosterItem.blocked_group()])
      Factory.insert(
        :roster_item,
        user_id: blocked_follower.id,
        subscription: :to,
        contact_id: shared.user.id)
      :ok
    end

    it "should return the full list of followers" do
      RosterItem.followers(shared.user.id)
      |> Enum.sort
      |> should(eq shared.contacts) # Does not include blocked follower
    end

    it "should not return users who aren't followers" do
      RosterItem.put(default_item(shared, subscription: :to))
      RosterItem.followers(shared.user.id)
      |> Enum.sort
      |> should(eq shared.contacts -- [shared.contact])
    end

    it "should return an empty list for non-users" do
      RosterItem.followers(ID.new) |> should(eq [])
    end

    it "should return an empty list for users with no contacts" do
      RosterItem.followers(shared.rosterless_user.id) |> should(eq [])
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
       following_list: Enum.sort([following_none, following_one])
      }
    end

    it "should return the full list of users being followed" do
      shared.following_none.id
      |> RosterItem.followees
      |> should(eq [])

      shared.following_one.id
      |> RosterItem.followees
      |> should(eq [shared.following_none])

      shared.following_two.id
      |> RosterItem.followees
      |> Enum.sort
      |> should(eq shared.following_list)
    end

    it "should return an empty list for non-users" do
      RosterItem.followees(ID.new) |> should(eq [])
    end
  end

  describe "friends/1" do
    before do
      blocked_friend = Factory.insert(:user, %{server: shared.server})
      Factory.insert(
        :roster_item,
        user_id: shared.user.id,
        contact_id: blocked_friend.id,
        groups: [RosterItem.blocked_group()])
      Factory.insert(
        :roster_item,
        user_id: blocked_friend.id,
        contact_id: shared.user.id)
      :ok
    end

    it "should return the full list of friends" do
      RosterItem.friends(shared.user.id)
      |> Enum.sort
      |> should(eq shared.contacts) # Does not include blocked friend
    end

    it "should not return users who aren't friends" do
      RosterItem.put(default_item(shared, subscription: :from))
      RosterItem.friends(shared.user.id)
      |> Enum.sort
      |> should(eq shared.contacts -- [shared.contact])
    end

    it "should return an empty list for non-users" do
      RosterItem.friends(ID.new) |> should(eq [])
    end

    it "should return an empty list for users with no contacts" do
      RosterItem.friends(shared.rosterless_user.id) |> should(eq [])
    end
  end

  describe "relationship/2" do
    it "should return :self when both user IDs are equal" do
      RosterItem.relationship(shared.user.id, shared.user.id)
      |> should(eq :self)
    end

    it "should return :friend where the two users are friends" do
      RosterItem.relationship(shared.user.id, shared.contact.id)
      |> should(eq :friend)

      RosterItem.relationship(shared.contact.id, shared.user.id)
      |> should(eq :friend)
    end

    it "should return :follower where user a is following user b" do
      RosterItem.relationship(shared.follower.id, shared.followee.id)
      |> should(eq :follower)
    end

    it "should return :followee where user b is following user a" do
      RosterItem.relationship(shared.followee.id, shared.follower.id)
      |> should(eq :followee)
    end

    it "should return :none if the users have no relationship" do
      RosterItem.relationship(shared.user.id, shared.rosterless_user.id)
      |> should(eq :none)

      RosterItem.relationship(shared.rosterless_user.id, shared.user.id)
      |> should(eq :none)
    end
  end

  describe "bump_all_versions/2" do
    it "should change the version for all roster entries with the contact" do
      initial = RosterItem.version(shared.user.id)
      RosterItem.bump_all_versions(shared.contact.id)
      |> should(eq :ok)
      RosterItem.version(shared.user.id) |> should(be :!=, initial)
    end

    it "should not change the data" do
      RosterItem.bump_all_versions(shared.contact.id)
      |> should(eq :ok)
      RosterItem.get(shared.user.id, shared.contact.id)
      |> Map.get(:contact)
      |> should(eq shared.contact)
    end
  end

  defp insert_roster_pair(user, contact, groups) do
    a = Factory.insert(
          :roster_item,
          user_id: user.id, contact_id: contact.id, groups: take_random(groups))
    b = Factory.insert(
          :roster_item,
          user_id: contact.id, contact_id: user.id, groups: take_random(groups))
    {a, b}
  end

  defp insert_follower_pair(follower, followee) do
    Factory.insert(
      :roster_item, subscription: :from,
      user_id: followee.id, contact_id: follower.id)
    Factory.insert(
      :roster_item, subscription: :to,
      user_id: follower.id, contact_id: followee.id)
  end

  defp take_random(list) do
    Enum.take_random(list, :rand.uniform(length(list)))
  end

  defp default_item(shared, replace) do
    r = %{user_id: shared.user.id,
          contact_id: shared.contact.id,
          name: Name.first_name,
          groups: [],
          ask: :none,
          subscription: :both}
    Map.merge(r, Map.new(replace))
  end
end
