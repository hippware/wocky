defmodule Wocky.AccountTest do
  use Wocky.DataCase

  alias Wocky.Account
  alias Wocky.Account.JWT.Client, as: ClientJWT
  alias Wocky.Account.JWT.Firebase
  alias Wocky.Account.JWT.Server, as: ServerJWT
  alias Wocky.Account.Token
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.User

  setup do
    user = Factory.insert(:user)
    {:ok, user: user}
  end

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

  describe "legacy token authentication" do
    setup %{user: user} do
      {:ok, {token, _}} = Token.assign(user.id, "testing")
      {:ok, token: token}
    end

    test "existing user with valid credentials", %{user: user, token: token} do
      assert {:ok, {^user, false}} = authenticate(:token, {user.id, token})
    end

    test "existing user with invalid credentials", %{user: user} do
      assert {:error, _} =
               authenticate(:token, {user.id(), Account.generate_token()})
    end

    test "non-existant user", %{user: user, token: token} do
      assert {:error, _} = authenticate(:token, {ID.new(), token})

      assert {:error, _} =
               authenticate(:token, {user.id, Account.generate_token()})
    end

    test "token revocation", %{user: user, token: token} do
      Account.release_token(user.id, "testing")

      assert {:error, _} = authenticate(:token, {user.id, token})
    end
  end

  describe "bypass authentication" do
    test "mismatched external id and phone number", %{user: user} do
      assert {:error, _} =
               authenticate(:bypass, {user.external_id, "+18005551234"})

      assert {:error, _} =
               authenticate(:bypass, {Factory.external_id(), "+18005551234"})
    end

    test "existing user", %{user: user} do
      assert {:ok, {_, false}} =
               authenticate(:bypass, {user.external_id, user.phone_number})

      assert {:ok, {_, false}} =
               authenticate(:bypass, {user.external_id, Factory.phone_number()})

      assert {:ok, {_, false}} =
               authenticate(:bypass, {Factory.external_id(), user.phone_number})
    end

    test "non-existant user" do
      assert {:ok, {_, true}} =
               authenticate(
                 :bypass,
                 {Factory.external_id(), Factory.phone_number()}
               )
    end
  end

  describe "firebase authentication" do
    test "existing user", %{user: user} do
      {:ok, token, _} = Firebase.encode_and_sign(user)

      assert {:ok, {_, false}} = authenticate(:firebase, token)
    end

    test "non-existant user" do
      new_user = Factory.build(:user)
      {:ok, token, _} = Firebase.encode_and_sign(new_user)

      assert {:ok, {_, true}} = authenticate(:firebase, token)
    end

    test "bogus token" do
      assert {:error, _} = authenticate(:firebase, "bogus")
    end
  end

  describe "client_jwt/bypass authentication" do
    test "existing user", %{user: user} do
      {:ok, token, _} = ClientJWT.encode_and_sign(user)

      assert {:ok, {_, false}} = authenticate(:client_jwt, token)
    end

    test "non-existant user" do
      new_user = Factory.build(:user)
      {:ok, token, _} = ClientJWT.encode_and_sign(new_user)

      assert {:ok, {_, true}} = authenticate(:client_jwt, token)
    end
  end

  describe "client_jwt/firebase authentication" do
    test "existing user", %{user: user} do
      {:ok, fb, _} = Firebase.encode_and_sign(user)
      {:ok, token, _} = ClientJWT.encode_and_sign(fb)

      assert {:ok, {_, false}} = authenticate(:client_jwt, token)
    end

    test "non-existant user" do
      new_user = Factory.build(:user)
      {:ok, fb, _} = Firebase.encode_and_sign(new_user)
      {:ok, token, _} = ClientJWT.encode_and_sign(fb)

      assert {:ok, {_, true}} = authenticate(:client_jwt, token)
    end
  end

  describe "client_jwt authentication" do
    test "bogus token" do
      assert {:error, _} = authenticate(:client_jwt, "bogus")
    end

    test "unknown wrapped credential" do
      jwk = %{"kty" => "oct", "k" => Base.encode64(ClientJWT.signing_key())}
      jws = %{"alg" => "HS512"}
      jwt = %{"iss" => "TinyRobot/1.0.0", "typ" => "bypass"}

      signed = JOSE.JWT.sign(jwk, jws, jwt)
      {_, token} = JOSE.JWS.compact(signed)

      assert {:error, _} = authenticate(:client_jwt, token)
    end
  end

  describe "server_jwt authentication" do
    test "existing user", %{user: user} do
      {:ok, token} = Account.get_location_jwt(user)

      assert {:ok, {_, false}} = authenticate(:server_jwt, token)
    end

    test "non-existant user" do
      user = Factory.build(:user)
      {:ok, token, _} = ServerJWT.encode_and_sign(user)

      assert {:error, _} = authenticate(:server_jwt, token)
    end

    test "bogus token" do
      assert {:error, _} = authenticate(:server_jwt, "bogus")
    end
  end

  describe "generic jwt authentication" do
    test "server_jwt token", %{user: user} do
      {:ok, token} = Account.get_location_jwt(user)

      assert {:ok, {_, false}} = authenticate(:jwt, token)
    end

    test "client_jwt firebase token", %{user: user} do
      {:ok, fb, _} = Firebase.encode_and_sign(user)
      {:ok, token, _} = ClientJWT.encode_and_sign(fb)

      assert {:ok, {_, false}} = authenticate(:jwt, token)
    end

    test "client_jwt bypass token", %{user: user} do
      {:ok, token, _} = ClientJWT.encode_and_sign(user)

      assert {:ok, {_, false}} = authenticate(:jwt, token)
    end

    test "unrecognized token" do
      jwk = %{"kty" => "oct", "k" => Base.encode64(ClientJWT.signing_key())}
      jws = %{"alg" => "HS512"}
      jwt = %{"iss" => "TinyRobot/1.0.0", "typ" => "bogus"}

      signed = JOSE.JWT.sign(jwk, jws, jwt)
      {_, token} = JOSE.JWS.compact(signed)

      assert {:error, _} = authenticate(:jwt, token)
    end

    test "bad token" do
      assert {:error, _} = authenticate(:jwt, "bogus")
    end
  end

  describe "unknown authentication" do
    test "fails" do
      assert {:error, _} = authenticate(:bogus, "")
    end
  end

  describe "account disabling" do
    test "when the user does not exist" do
      assert Account.disable_user(ID.new()) == :ok
    end

    test "when the user exists" do
      user = Factory.insert(:user, resource: "testing")
      {:ok, {token, _}} = Token.assign(user.id, "testing")

      Account.disable_user(user.id)

      refute Token.valid?(user.id, token)

      disabled_user = Repo.get(User, user.id)
      assert disabled_user.phone_number == nil
      assert disabled_user.provider == nil
      assert disabled_user.external_id == nil
    end
  end
end
