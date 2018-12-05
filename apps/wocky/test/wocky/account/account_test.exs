defmodule Wocky.AccountTest do
  use Wocky.DataCase

  alias Wocky.Account
  alias Wocky.Account.JWT.Client, as: ClientJWT
  alias Wocky.Account.JWT.Firebase
  alias Wocky.Account.JWT.Server, as: ServerJWT
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.User

  setup do
    user = Factory.insert(:user)
    {:ok, id: user.id, user: user}
  end

  defp make_client_token(sub) do
    {:ok, token, _} = ClientJWT.encode_and_sign(sub, %{"dvc" => "testing"})
    token
  end

  describe "bypass authentication" do
    test "existing user with bypass number", %{user: user, id: id} do
      token = make_client_token(user)

      assert {:ok, %User{id: ^id}} = Account.authenticate(token)
    end

    test "existing user without bypass number" do
      user = Factory.insert(:user, phone_number: "+18001234567")
      token = make_client_token(user)

      assert {:error, _} = Account.authenticate(token)
    end

    test "non-existant user" do
      new_user = Factory.build(:user)
      token = make_client_token(new_user)

      id = new_user.external_id
      assert {:ok, %User{external_id: ^id}} = Account.authenticate(token)
    end
  end

  describe "firebase authentication" do
    test "existing user", %{user: user, id: id} do
      {:ok, fb, _} = Firebase.encode_and_sign(user)
      token = make_client_token(fb)

      assert {:ok, %User{id: ^id}} = Account.authenticate(token)
    end

    test "non-existant user" do
      new_user = Factory.build(:user)
      {:ok, fb, _} = Firebase.encode_and_sign(new_user)
      token = make_client_token(fb)

      id = new_user.external_id
      assert {:ok, %User{external_id: ^id}} = Account.authenticate(token)
    end

    test "bogus token" do
      token = make_client_token("bogus")

      assert {:error, _} = Account.authenticate(token)
    end
  end

  describe "authentication errors" do
    test "bogus token" do
      assert {:error, _} = Account.authenticate("bogus")
    end

    test "unknown wrapped credential" do
      jwk = %{"kty" => "oct", "k" => Base.encode64(ClientJWT.signing_key())}
      jws = %{"alg" => "HS512"}
      jwt = %{"iss" => "TinyRobot/1.0.0", "typ" => "bypass"}

      signed = JOSE.JWT.sign(jwk, jws, jwt)
      {_, token} = JOSE.JWS.compact(signed)

      assert {:error, _} = Account.authenticate(token)
    end
  end

  describe "location authentication" do
    test "existing user with server_jwt token", %{user: user, id: id} do
      {:ok, token} = Account.get_location_jwt(user)

      assert {:ok, %User{id: ^id}} = Account.authenticate_for_location(token)
    end

    test "non-existant user" do
      user = Factory.build(:user)
      {:ok, token, _} = ServerJWT.encode_and_sign(user)

      assert {:error, _} = Account.authenticate_for_location(token)
    end

    test "client_jwt firebase token", %{user: user, id: id} do
      {:ok, fb, _} = Firebase.encode_and_sign(user)
      token = make_client_token(fb)

      assert {:ok, %User{id: ^id}} = Account.authenticate_for_location(token)
    end

    test "client_jwt bypass token", %{user: user, id: id} do
      token = make_client_token(user)

      assert {:ok, %User{id: ^id}} = Account.authenticate_for_location(token)
    end

    test "unrecognized token" do
      jwk = %{"kty" => "oct", "k" => Base.encode64(ClientJWT.signing_key())}
      jws = %{"alg" => "HS512"}
      jwt = %{"iss" => "TinyRobot/1.0.0", "typ" => "bogus"}

      signed = JOSE.JWT.sign(jwk, jws, jwt)
      {_, token} = JOSE.JWS.compact(signed)

      assert {:error, _} = Account.authenticate_for_location(token)
    end

    test "bad token" do
      assert {:error, _} = Account.authenticate_for_location("bogus")
    end
  end

  describe "account disabling" do
    test "when the user does not exist" do
      assert Account.disable_user(ID.new()) == :ok
    end

    test "when the user exists" do
      user = Factory.insert(:user, device: "testing")

      Account.disable_user(user.id)

      disabled_user = Repo.get(User, user.id)
      assert disabled_user.phone_number == nil
      assert disabled_user.provider == nil
      assert disabled_user.external_id == nil
    end
  end
end
