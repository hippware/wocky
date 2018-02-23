defmodule Wocky.AccountsTest do
  use Wocky.DataCase

  alias Wocky.Account
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.User

  @test_server "localhost"
  @required_attrs [:username, :server, :external_id]

  describe "changeset validations" do
    test "should pass with valid attributes" do
      id = ID.new()

      changeset = Account.changeset(%{
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
      changeset = Account.changeset(%{
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

  describe "registration with external auth" do
    setup do
      user = Factory.insert(:user, resource: "testing")

      {:ok,
       id: user.id,
       external_id: user.external_id,
       phone_number: user.phone_number}
    end

    test "when the user already exists with the same provider/extID",
        %{id: id, external_id: external_id} do
      assert {:ok, {^id, @test_server, false}} =
        Account.register_external(
          "another_server",
          "local",
          external_id,
          "+15551234567"
        )
    end

    test "when the user already exists with the same phone number",
        %{id: id, phone_number: phone_number} do
      external_id = Faker.Code.isbn13()

      assert {:ok, {^id, @test_server, false}} =
        Account.register_external(
          "another_server",
          "firebase",
          external_id,
          phone_number
        )

      user = Repo.get_by(User, id: id)
      assert user.provider == "firebase"
      assert user.external_id == external_id
    end

    test "when the user does not exist" do
      assert {:ok, {result_id, @test_server, true}} =
        Account.register_external(
          @test_server,
          "firebase",
          ID.new(),
          "+15551234567"
        )

      assert Repo.get(User, result_id)
    end
  end
end
