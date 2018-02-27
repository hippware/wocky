defmodule Wocky.Roster.InitialContactSpec do
  use ESpec, async: true
  use ModelHelpers

  alias Wocky.Roster.InitialContact
  alias Wocky.Repo

  before do
    init_contacts =
      5
      |> Factory.insert_list(:user)
      |> Enum.map(fn c ->
        :initial_contact
        |> Factory.insert(user_id: c.id)
        |> Repo.preload(:user)
      end)
      |> Enum.sort()

    {:ok, init_contacts: init_contacts}
  end

  describe "get/0" do
    it "should return all initial contacts" do
      InitialContact.get() |> Enum.sort() |> should(eq shared.init_contacts)
    end
  end

  describe "put/2" do
    before do
      user = Factory.insert(:user)
      result = InitialContact.put(user, :friend)
      {:ok, user: user, result: result}
    end

    it "should return :ok" do
      shared.result |> should(eq :ok)
    end

    it "should insert an item into the db" do
      new_contact = (InitialContact.get() -- shared.init_contacts) |> hd
      new_contact |> should(have user: shared.user)
      new_contact |> should(have type: :friend)
    end
  end
end
