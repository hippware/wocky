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
    Enum.each(contacts, &insert_roster_pair(user, &1, groups))

    user2 = Factory.insert(:user, %{server: shared.server})

    {:ok,
     user: user,
     contacts: Enum.sort(contacts),
     contact: hd(contacts),
     rosterless_user: user2,
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

    it "should return true if the contact has 'from' subscription" do
      RosterItem.put(default_item(shared, subscription: :from))
      RosterItem.is_follower(shared.user.id, shared.contact.id)
      |> should(be_true())
    end

    it "should return false if the contact does not have 'both' or 'from' subscription" do
      RosterItem.put(default_item(shared, subscription: :to))
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

  describe "followers/1" do
    it "should return the full list of followers" do
      RosterItem.followers(shared.user.id)
      |> Enum.map(&Map.get(&1, :contact))
      |> Enum.sort
      |> should(eq shared.contacts)
    end

    it "should not return users who aren't followers" do
      RosterItem.put(default_item(shared, subscription: :to))
      RosterItem.followers(shared.user.id)
      |> Enum.map(&Map.get(&1, :contact))
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

  describe "friends/1" do
    it "should return the full list of friends" do
      RosterItem.friends(shared.user.id)
      |> Enum.map(&Map.get(&1, :contact))
      |> Enum.sort
      |> should(eq shared.contacts)
    end

    it "should not return users who aren't friends" do
      RosterItem.put(default_item(shared, subscription: :from))
      RosterItem.friends(shared.user.id)
      |> Enum.map(&Map.get(&1, :contact))
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

  describe "bump_version/2" do
    it "should change the version" do
      initial = RosterItem.version(shared.user.id)
      RosterItem.bump_version(shared.user.id, shared.contact.id)
      |> should(eq :ok)
      RosterItem.version(shared.user.id) |> should(be :!=, initial)
    end

    it "should not change the data" do
      RosterItem.bump_version(shared.user.id, shared.contact.id)
      |> should(eq :ok)
      RosterItem.get(shared.user.id, shared.contact.id)
      |> Map.get(:contact)
      |> should(eq shared.contact)
    end
  end


  defp insert_roster_pair(user, contact, groups) do
    Factory.insert(
      :roster_item,
      user_id: user.id, contact_id: contact.id, groups: take_random(groups))
    Factory.insert(
      :roster_item,
      user_id: contact.id, contact_id: user.id, groups: take_random(groups))
  end

  defp take_random(list) do
    Enum.take_random(list, :rand.uniform(length(list)))
  end

  defp default_item(shared, replace) do
    r = %{user_id: shared.user.id,
          contact_id: shared.contact.id,
          name: Name.first_name,
          groups: [],
          ask: :out,
          subscription: :both}
    Map.merge(r, Map.new(replace))
  end
end
