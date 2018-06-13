defmodule Wocky.Account.RegisterTest do
  use Wocky.DataCase

  alias Timex.Duration
  alias Wocky.Account.Register
  alias Wocky.HomeStream
  alias Wocky.Repo
  alias Wocky.Repo.{ID, Factory}
  alias Wocky.Roster
  alias Wocky.Roster.Item
  alias Wocky.User

  @required_attrs [:username, :external_id]

  @create_attrs %{
    username: "bed9f0d7-2db2-47df-88a1-830749a44f5a",
    provider: "test_provider",
    external_id: "1234567890",
    phone_number: "+12104445484",
    password: "password",
    pass_details: "details"
  }

  describe "changeset/1" do
    test "should pass with valid attributes" do
      id = ID.new()

      changeset =
        Register.changeset(%{
          username: id,
          provider: "local",
          external_id: "bar"
        })

      assert changeset.valid?
      assert changeset.changes.id == changeset.changes.username
      assert changeset.changes.id == id
    end

    test "should fail if missing required attributes" do
      changeset = Register.changeset(%{})
      refute changeset.valid?

      for a <- @required_attrs do
        assert "can't be blank" in errors_on(changeset)[a]
      end
    end

    test "should fail with an invalid username" do
      changeset =
        Register.changeset(%{
          username: "alice",
          external_id: "bar"
        })

      refute changeset.valid?
      assert errors_on(changeset)[:username]
    end
  end

  describe "get_external_id/1" do
    test "when the user has an external id" do
      user = Factory.build(:user)
      assert Register.get_external_id(user) == user.external_id
    end

    test "when the user does not have an external id" do
      user = Factory.insert(:user, external_id: nil)
      external_id = Register.get_external_id(user)

      assert external_id

      user2 = Repo.get(User, user.id)
      assert user2.external_id == external_id
    end
  end

  describe "find/3" do
    setup do
      user = Factory.insert(:user)
      {:ok, user: user}
    end

    test "when the user does not exist" do
      assert {:error, :not_found} = Register.find("foo", "bar", "baz")
    end

    test "finding user by external id", %{user: u} do
      assert {:ok, user} = Register.find(u.provider, u.external_id, "foo")
      assert user.phone_number == u.phone_number
    end

    test "finding user by phone number", %{user: u} do
      assert {:ok, user} = Register.find("testp", "testid", u.phone_number)
      assert user.provider == "testp"
      assert user.external_id == "testid"
    end
  end

  describe "create/2" do
    test "with valid attributes" do
      assert {:ok, user} = Register.create(@create_attrs)

      user_attrs = Map.from_struct(user)
      for {k, v} <- @create_attrs, do: assert(user_attrs[k] == v)
    end

    test "with defaults" do
      assert {:ok, user} = Register.create(%{})
      assert user.username
      assert user.external_id
      assert user.provider == "local"
    end
  end

  defp setup_initial_contacts(type) do
    3
    |> Factory.insert_list(:user)
    |> Enum.map(fn u ->
      Factory.insert(:initial_contact, user_id: u.id, type: type)

      sub =
        case type do
          :followee -> :to
          :follower -> :from
          :friend -> :both
        end

      {u, sub}
    end)
  end

  describe "create/2 user prepopulation" do
    setup do
      initial_contacts =
        for type <- [:followee, :follower, :friend] do
          setup_initial_contacts(type)
        end

      prepop_user =
        Factory.insert(
          :user,
          handle: HomeStream.prepopulation_user(),
          roles: [User.no_index_role(), User.system_role()]
        )

      old_ts = Timex.subtract(DateTime.utc_now(), Duration.from_weeks(6))

      Factory.insert_list(
        15,
        :home_stream_item,
        user_id: prepop_user.id,
        created_at: old_ts,
        updated_at: old_ts
      )

      prepop_items =
        Factory.insert_list(15, :home_stream_item, user_id: prepop_user.id)

      {:ok, user} = Register.create(@create_attrs, true)

      {:ok,
       user: user,
       initial_contacts: List.flatten(initial_contacts),
       prepop_items: prepop_items}
    end

    test "initial contacts", %{user: user, initial_contacts: init_contacts} do
      roster = Roster.get(user.id)

      assert length(init_contacts) == length(roster)

      for {contact, sub} <- init_contacts do
        item = Enum.find(roster, fn %Item{contact: c} -> contact.id == c.id end)

        assert item
        assert item.subscription == sub
        assert Enum.member?(item.groups, "__new__")
      end
    end

    test "home stream", %{user: user, prepop_items: prepop_items} do
      hs = HomeStream.get(user.id)
      assert length(hs) == length(prepop_items)
    end
  end

  describe "find_or_create/4" do
    setup do
      user = Factory.insert(:user)
      {:ok, id: user.id, user: user}
    end

    test "when a user with the same provider/ID exists", %{user: user} do
      assert {:ok, {%User{} = new_user, false}} =
               Register.find_or_create(
                 user.provider,
                 user.external_id,
                 Factory.phone_number()
               )

      assert new_user.id == user.id
      assert new_user.provider == user.provider
      assert new_user.external_id == user.external_id
      assert new_user.phone_number == user.phone_number
    end

    test "when a user with the same phone number exists", %{user: user} do
      external_id = Factory.external_id()

      assert {:ok, {%User{} = new_user, false}} =
               Register.find_or_create(
                 "test_provider",
                 external_id,
                 user.phone_number
               )

      assert new_user.id == user.id
      assert new_user.provider == "test_provider"
      assert new_user.external_id == external_id
      assert new_user.phone_number == user.phone_number
    end

    test "when the user does not exist" do
      external_id = Factory.external_id()
      phone_number = Factory.phone_number()

      assert {:ok, {%User{} = new_user, true}} =
               Register.find_or_create(
                 "test_provider",
                 external_id,
                 phone_number
               )

      assert new_user.provider == "test_provider"
      assert new_user.external_id == external_id
      assert new_user.phone_number == phone_number
    end
  end
end
