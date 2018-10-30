defmodule Wocky.Roster.InitialContactTest do
  use Wocky.DataCase, async: true

  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Roster.InitialContact

  setup do
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
    test "should return all initial contacts", ctx do
      assert Enum.sort(InitialContact.get()) == ctx.init_contacts
    end
  end

  describe "put/2" do
    setup do
      user = Factory.insert(:user)
      result = InitialContact.put(user, :friend)
      {:ok, user: user, result: result}
    end

    test "should return :ok", ctx do
      assert ctx.result == :ok
    end

    test "should insert an item into the db", %{user: user} = ctx do
      assert %{user: ^user, type: :friend} =
               hd(InitialContact.get() -- ctx.init_contacts)
    end
  end
end
