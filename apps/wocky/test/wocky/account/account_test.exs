defmodule Wocky.AccountTest do
  use Wocky.DataCase

  alias Wocky.Account
  alias Wocky.Account.{ClientJWT, Firebase, Token}
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.User

  describe "password registration" do
    test "should create a user" do
      id = ID.new()
      {:ok, _} = Account.register(id, "password", "password")

      new_user = Repo.get(User, id)

      assert new_user
      assert new_user.password == "password"
      assert new_user.pass_details == "password"
    end
  end

  defp authenticate(type, creds) do
    Account.authenticate(type, creds)
  end

  describe "authentication" do
    setup do
      user = Factory.insert(:user)
      {:ok, user: user}
    end

    test "token authentication", %{user: user} do
      {:ok, {token, _}} = Token.assign(user.id, "testing")

      assert {:ok, {user, false}} == authenticate(:token, {user.id, token})

      assert {:error, "Invalid token"} ==
               authenticate(:token, {ID.new(), token})

      assert {:error, "Invalid token"} ==
               authenticate(:token, {user.id(), Token.generate()})
    end

    test "bypass authentication", %{user: user} do
      assert {:error, _} =
               authenticate(:bypass, {user.external_id, "+18005551234"})

      assert {:error, _} =
               authenticate(:bypass, {Factory.external_id(), "+18005551234"})

      assert {:ok, {_, false}} =
               authenticate(:bypass, {user.external_id, user.phone_number})

      assert {:ok, {_, false}} =
               authenticate(:bypass, {user.external_id, Factory.phone_number()})

      assert {:ok, {_, false}} =
               authenticate(:bypass, {Factory.external_id(), user.phone_number})

      assert {:ok, {_, true}} =
               authenticate(
                 :bypass,
                 {Factory.external_id(), Factory.phone_number()}
               )
    end

    test "firebase authentication", %{user: user} do
      {:ok, token, _} = Firebase.encode_and_sign(user)

      assert {:ok, {_, false}} = authenticate(:firebase, token)

      new_user = Factory.build(:user)
      {:ok, token, _} = Firebase.encode_and_sign(new_user)

      assert {:ok, {_, true}} = authenticate(:firebase, token)
    end

    test "client_jwt/bypass authentication", %{user: user} do
      {:ok, token, _} = ClientJWT.encode_and_sign(user)

      assert {:ok, {_, false}} = authenticate(:client_jwt, token)

      new_user = Factory.build(:user)
      {:ok, token, _} = ClientJWT.encode_and_sign(new_user)

      assert {:ok, {_, true}} = authenticate(:client_jwt, token)
    end

    test "client_jwt/firebase authentication", %{user: user} do
      {:ok, fb, _} = Firebase.encode_and_sign(user)
      {:ok, token, _} = ClientJWT.encode_and_sign(fb)

      assert {:ok, {_, false}} = authenticate(:client_jwt, token)

      new_user = Factory.build(:user)
      {:ok, fb, _} = Firebase.encode_and_sign(new_user)
      {:ok, token, _} = ClientJWT.encode_and_sign(fb)

      assert {:ok, {_, true}} = authenticate(:client_jwt, token)
    end

    test "bogus authentication provider" do
      assert {:error, _} = authenticate(:bogus, "")
    end
  end

  describe "account disabling" do
    test "when the user does not exist" do
      assert Account.disable_user(ID.new()) == :ok
    end

    test "when the user exists" do
      user = Factory.insert(:user, device: "testing")
      {:ok, {token, _}} = Token.assign(user.id, user.device)

      Account.disable_user(user.id)

      refute Token.valid?(user.id, token)

      disabled_user = Repo.get(User, user.id)
      assert disabled_user.phone_number == nil
      assert disabled_user.provider == nil
      assert disabled_user.external_id == nil
    end
  end
end
