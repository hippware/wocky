defmodule Wocky.Push.TokenTest do
  use Wocky.DataCase, async: true

  alias Wocky.Push.Token

  @attrs [:user_id, :resource, :platform, :token]

  test "required attributes" do
    changeset = Token.register_changeset(%{})
    refute changeset.valid?
    for a <- @attrs do
      assert "can't be blank" in errors_on(changeset)[a]
    end
  end

  test "valid platform attribute" do
    changeset = Token.register_changeset(%{platform: "apple"})
    assert changeset |> errors_on |> Map.get(:platform) |> is_nil
  end

  test "invalid platform attribute" do
    changeset = Token.register_changeset(%{platform: "bogus"})
    assert "is invalid" in errors_on(changeset).platform
  end
end
