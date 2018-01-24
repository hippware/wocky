defmodule Wocky.Push.TokenTest do
  use Wocky.DataCase, async: true

  alias Wocky.Push.Token

  @attrs [:user_id, :resource, :token]

  test "required attributes" do
    changeset = Token.register_changeset(%{})
    refute changeset.valid?

    for a <- @attrs do
      assert "can't be blank" in errors_on(changeset)[a]
    end
  end
end
