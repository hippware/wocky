defmodule Wocky.AccountsTest do
  use Wocky.DataCase

  alias Wocky.Account
  alias Wocky.Account.Token
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.User

  @test_server "localhost"
  @required_attrs [:username, :server, :external_id]

  describe "changeset validations" do
    test "should pass with valid attributes" do
      id = ID.new()

      changeset =
        Account.changeset(%{
          username: id,
          server: "foo",
          provider: "local",
          external_id: "bar"
        })

      assert changeset.valid?
      assert changeset.changes.id == changeset.changes.username
      assert changeset.changes.id == id
    end

    test "should fail if missing required attributes" do
      changeset = Account.changeset(%{})
      refute changeset.valid?

      for a <- @required_attrs do
        assert "can't be blank" in errors_on(changeset)[a]
      end
    end

    test "should fail with an invalid username" do
      changeset =
        Account.changeset(%{
          username: "alice",
          server: "foo",
          external_id: "bar"
        })

      refute changeset.valid?
      assert errors_on(changeset)[:username]
    end
  end

  describe "password registration" do
    setup do
      id = ID.new()
      result = Account.register(id, @test_server, "password", "password")
      {:ok, id: id, result: result}
    end

    test "should return a success result", %{result: result} do
      assert {:ok, _} = result
    end

    test "should create a user", %{id: id} do
      new_user = Repo.get(User, id)

      assert new_user
      assert new_user.server == @test_server
      assert new_user.password == "password"
      assert new_user.pass_details == "password"
    end
  end

  describe "post authentication process" do
    setup do
      user = Factory.insert(:user, resource: "testing")
      {:ok, id: user.id, user: user}
    end

    test "when a user with the same provider/ID exists", %{user: user} do
      assert {:ok, {%User{} = new_user, false}} =
               Account.on_authenticated(
                 "a_server",
                 user.provider,
                 user.external_id,
                 Factory.phone_number()
               )

      assert new_user.id == user.id
      assert new_user.server == user.server
      assert new_user.provider == user.provider
      assert new_user.external_id == user.external_id
      assert new_user.phone_number == user.phone_number
    end

    test "when a user with the same phone number exists", %{user: user} do
      external_id = Faker.Code.isbn13()

      assert {:ok, {%User{} = new_user, false}} =
               Account.on_authenticated(
                 "a_server",
                 "test_provider",
                 external_id,
                 user.phone_number
               )

      assert new_user.id == user.id
      assert new_user.server == user.server
      assert new_user.provider == "test_provider"
      assert new_user.external_id == external_id
      assert new_user.phone_number == user.phone_number
    end

    test "when the user does not exist" do
      external_id = Faker.Code.isbn13()
      phone_number = Factory.phone_number()

      assert {:ok, {%User{} = new_user, true}} =
               Account.on_authenticated(
                 "a_server",
                 "test_provider",
                 external_id,
                 phone_number
               )

      assert new_user.server == "a_server"
      assert new_user.provider == "test_provider"
      assert new_user.external_id == external_id
      assert new_user.phone_number == phone_number
    end
  end

  describe "account disabling" do
    test "when the user does not exist" do
      assert Account.disable_user(ID.new()) == :ok
    end

    test "when the user exists" do
      user = Factory.insert(:user, resource: "testing")
      {:ok, {token, _}} = Token.assign(user.id, "testing")
      assert Token.valid?(user.id, token)

      Account.disable_user(user.id)

      assert not Token.valid?(user.id, token)
      disabled_user = Repo.get(User, user.id)
      assert disabled_user.phone_number == nil
      assert disabled_user.provider == nil
      assert disabled_user.external_id == nil
    end
  end
end
