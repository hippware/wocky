defmodule Wocky.Account.JWT.ClientTest do
  use Wocky.DataCase

  alias Wocky.Account.JWT.Client, as: ClientJWT
  alias Wocky.Account.JWT.Firebase
  alias Wocky.Repo.Factory

  setup do
    user = Factory.insert(:user)
    {:ok, user: user}
  end

  describe "subject_for_token/2" do
    test "with a valid user", %{user: user} do
      {:ok, subject} = ClientJWT.subject_for_token(user, %{})
      assert subject == user.external_id
    end

    test "with a Firebase token" do
      {:ok, subject} = ClientJWT.subject_for_token("ThisIsAToken", %{})
      assert subject == "ThisIsAToken"
    end

    test "with any other data" do
      assert {:error, :unknown_resource} ==
               ClientJWT.subject_for_token(%{}, %{})
    end
  end

  describe "encode_and_sign/1" do
    test "should include external_id and phone_number for bypass", %{user: u} do
      {:ok, _, claims} = ClientJWT.encode_and_sign(u)
      assert claims["typ"] == "bypass"
      assert claims["sub"] == u.external_id
      assert claims["phone_number"] == u.phone_number
    end

    test "should include token for firebase" do
      {:ok, _, claims} = ClientJWT.encode_and_sign("ThisIsAToken")
      assert claims["typ"] == "firebase"
      assert claims["sub"] == "ThisIsAToken"
    end
  end

  describe "resource_from_claims/1" do
    test "finds a user when proper bypass fields exist", %{user: user} do
      claims = %{
        "typ" => "bypass",
        "sub" => user.external_id,
        "phone_number" => user.phone_number
      }

      {:ok, new_user} = ClientJWT.resource_from_claims(claims)

      assert new_user.id == user.id
      assert new_user.external_id == user.external_id
      assert new_user.phone_number == user.phone_number
    end

    test "finds a user when proper firebase fields exist", %{user: user} do
      {:ok, token, _} = Firebase.encode_and_sign(user)

      claims = %{
        "typ" => "firebase",
        "sub" => token
      }

      {:ok, new_user} = ClientJWT.resource_from_claims(claims)

      assert new_user.id == user.id
      assert new_user.external_id == user.external_id
      assert new_user.phone_number == user.phone_number
    end

    test "returns an error when required fields are missing" do
      assert {:error, :not_possible} == ClientJWT.resource_from_claims(%{})
    end
  end

  describe "resource_from_token/1" do
    test "bypass returns user and identical claims", %{user: user} do
      {:ok, token, claims1} = ClientJWT.encode_and_sign(user)
      {:ok, new_user, claims2} = ClientJWT.resource_from_token(token)

      assert claims1 == claims2
      assert new_user.id == user.id
      assert new_user.external_id == user.external_id
      assert new_user.phone_number == user.phone_number
    end

    test "firebase returns user and identical claims", %{user: user} do
      {:ok, fb, _} = Firebase.encode_and_sign(user)

      {:ok, token, claims1} = ClientJWT.encode_and_sign(fb)
      {:ok, new_user, claims2} = ClientJWT.resource_from_token(token)

      assert claims1 == claims2
      assert new_user.id == user.id
      assert new_user.external_id == user.external_id
      assert new_user.phone_number == user.phone_number
    end
  end

  describe "verify_claims/3" do
    @valid_claims %{
      "iss" => "TinyRobot/1.0.0",
      "typ" => "firebase",
      "aud" => "Wocky"
    }

    defp verify_claim(key, value) do
      claims = Map.put(@valid_claims, key, value)
      {:ok, claims} == ClientJWT.verify_claims(claims, %{})
    end

    test "iss validation" do
      assert verify_claim("iss", "TinyRobot/1.0.0")
      assert verify_claim("iss", "TinyRobot/1.0.0 (Testing)")
      assert verify_claim("iss", "TinyRobot/1.0.0 (Wocky; Testing)")
      refute verify_claim("iss", "Foo")
      refute verify_claim("iss", nil)
    end

    test "aud validation" do
      assert verify_claim("aud", nil)
      assert verify_claim("aud", "Wocky")
      refute verify_claim("aud", "Anything Else")
    end

    test "typ validation" do
      assert verify_claim("typ", "bypass")
      assert verify_claim("typ", "firebase")
      refute verify_claim("typ", "foo")
      refute verify_claim("typ", nil)
    end
  end
end
