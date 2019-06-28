defmodule Wocky.Account.JWT.ServerTest do
  use Wocky.DataCase, async: true

  alias Wocky.Account.JWT.Server, as: ServerJWT
  alias Wocky.Account.User
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID

  setup do
    user = Factory.insert(:user)
    {:ok, user: user}
  end

  describe "subject_for_token/2" do
    test "with a valid user", %{user: user} do
      {:ok, subject} = ServerJWT.subject_for_token(user, %{})
      assert subject == user.id
    end

    test "with any other data" do
      assert {:error, :unknown_resource} ==
               ServerJWT.subject_for_token(%{}, %{})
    end
  end

  describe "resource_from_claims/1" do
    test "finds a user when proper sub field exists", %{user: %{id: id} = user} do
      assert {:ok, %User{id: ^id}} =
               ServerJWT.resource_from_claims(%{"sub" => user.id})
    end

    test "returns an error with a bad sub field" do
      assert {:error, :not_found} =
               ServerJWT.resource_from_claims(%{"sub" => ID.new()})
    end

    test "returns an error when required fields are missing" do
      assert {:error, :not_possible} == ServerJWT.resource_from_claims(%{})
    end
  end
end
