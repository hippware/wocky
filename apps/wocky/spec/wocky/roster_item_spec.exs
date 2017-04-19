defmodule Wocky.RosterItemSpec do
  use ESpec, async: true

  alias Faker.Lorem
  alias Faker.Name
  alias Wocky.RosterGroup
  alias Wocky.RosterItem
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID

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
     contacts: contacts,
     contact_ids: Enum.map(contacts, &Map.get(&1, :id)),
     rosterless_user: user2,
     groups: groups}
  end

  describe "find/1" do
    it "should return all roster items for a user" do
      RosterItem.find(shared.user.id)
      |> Enum.map(&Map.get(&1, :contact))
      |> should(eq shared.contacts)
    end

    it "should return an empty list for a user with no roster items" do
      RosterItem.find(shared.rosterless_user.id) |> should(eq [])
    end

    it "should return an empty list for a non-existant user" do
      RosterItem.find(ID.new) |> should(eq [])
    end
  end

  describe "find/2" do
    it "should return the roster item for the specified contact" do
      Enum.map shared.contacts, fn(c) ->
        RosterItem.find(shared.user.id, c.id)
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
        RosterItem.put(shared.user.id, contact.id, name, groups, :out, :both)
        |> should(eq :ok)

        item = RosterItem.find(shared.user.id, contact.id)
        item.contact |> should(eq contact)
        item.name |> should(eq name)
        item.ask |> should(eq :out)
        item.subscription |> should(eq :both)
        item.groups |> should(have_count length(groups))
      end
    end

    context "when there is an existing entry for the contact" do
      before do
        setup_pair(shared)
      end

      it "should update the existing contact" do
        new_name = Name.first_name
        RosterItem.put(shared.user.id, shared.contact.id, new_name,
                       [shared.group], :out, :both)
        |> should(eq :ok)

        item = RosterItem.find(shared.user.id, shared.contact.id)
        item.contact |> should(eq shared.contact)
        item.name |> should(eq new_name)
        item.ask |> should(eq :out)
        item.subscription |> should(eq :both)
        item.groups |> hd |> should(eq shared.group)
      end
    end
  end

  describe "version/1" do
    before do
      setup_pair(shared)
    end

    it "should return the version for the roster" do
      RosterItem.version(shared.user.id)
      |> should(be_integer())
    end

    it "should return 0 for a user with no roster items" do
      RosterItem.version(shared.rosterless_user.id) |> should(eq 0)
    end

    it "should return 0 for a non-existant user" do
      RosterItem.version(ID.new) |> should(eq 0)
    end

    it "should change when the roster is written to" do
      initial = RosterItem.version(shared.user.id)

      RosterItem.put(shared.user.id, shared.contact.id, Name.first_name,
                     [], :out, :both)

      RosterItem.version(shared.user.id) |> should(be :!=, initial)
    end
  end

  describe "delete/2" do
    before do
      setup_pair(shared)
    end

    it "should remove the contact from the user's roster" do
      RosterItem.delete(shared.user.id, shared.contact.id) |> should(eq :ok)
      RosterItem.find(shared.user.id, shared.contact.id) |> should(eq nil)
    end

    it "should change the roster version" do
      initial = RosterItem.version(shared.user.id)
      RosterItem.delete(shared.user.id, shared.contact.id) |> should(eq :ok)
      RosterItem.version(shared.user.id) |> should(be :!=, initial)
    end
  end

  describe "users_with_contact/1" do
    it "should return the count of users with a given contact" do
      RosterItem.users_with_contact(shared.user.id)
      |> should(eq shared.contact_ids)
      RosterItem.users_with_contact(hd(shared.contacts).id)
      |> should(eq [shared.user.id])
    end

    it "should return 0 for a non-existant user" do
      RosterItem.users_with_contact(ID.new) |> should(eq [])
    end

    it "should return 0 for a user with no contacts" do
      user = Factory.insert(:user, %{server: shared.server})
      RosterItem.users_with_contact(user.id) |> should(eq [])
    end
  end


  defp setup_pair(shared) do
    user = Factory.insert(:user, %{server: shared.server})
    contact = Factory.insert(:user, %{server: shared.server})
    group = Lorem.word
    insert_roster_pair(user, contact, [group])
    {:ok, user: user, contact: contact, group: group}
  end

  defp insert_roster_pair(user, contact, groups) do
    Factory.insert(
      :roster_item,
      user_id: user.id, contact_id: contact.id, groups: make_groups(groups))
    Factory.insert(
      :roster_item,
      user_id: contact.id, contact_id: user.id, groups: make_groups(groups))
  end

  defp make_groups(groups) do
    groups
    |> take_random()
    |> Enum.join(<<0>>)
  end

  defp take_random(groups) do
    Enum.take_random(groups, :rand.uniform(length(groups)))
  end
end
