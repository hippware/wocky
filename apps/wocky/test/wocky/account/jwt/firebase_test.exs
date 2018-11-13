defmodule Wocky.Account.JWT.FirebaseTest do
  use Wocky.DataCase

  alias Wocky.Account.JWT.Firebase
  alias Wocky.Repo.Factory

  setup do
    user = Factory.insert(:user)
    {:ok, user: user}
  end

  describe "subject_for_token/2" do
    test "with a valid user", %{user: user} do
      {:ok, subject} = Firebase.subject_for_token(user, %{})
      assert subject == user.external_id
    end

    test "with any other data" do
      assert {:error, :unknown_resource} == Firebase.subject_for_token(%{}, %{})
    end
  end

  describe "encode_and_sign/1" do
    test "should include external_id and phone_number", %{user: user} do
      {:ok, _, claims} = Firebase.encode_and_sign(user)
      assert claims["sub"] == user.external_id
      assert claims["phone_number"] == user.phone_number
    end
  end

  describe "resource_from_claims/1" do
    test "finds a user when proper fields exist", %{user: user} do
      claims = %{
        "sub" => user.external_id,
        "phone_number" => user.phone_number
      }

      {:ok, new_user} = Firebase.resource_from_claims(claims)

      assert new_user.id == user.id
      assert new_user.external_id == user.external_id
      assert new_user.phone_number == user.phone_number
    end

    test "returns an error when required fields are missing" do
      assert {:error, :not_possible} == Firebase.resource_from_claims(%{})
    end
  end

  describe "resource_from_token/1" do
    test "returns user and identical claims", %{user: user} do
      {:ok, token, claims1} = Firebase.encode_and_sign(user)
      {:ok, new_user, claims2} = Firebase.resource_from_token(token)

      assert claims1 == claims2
      assert new_user.id == user.id
      assert new_user.external_id == user.external_id
      assert new_user.phone_number == user.phone_number
    end
  end
end
