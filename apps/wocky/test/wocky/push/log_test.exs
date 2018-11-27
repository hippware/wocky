defmodule Wocky.Push.LogTest do
  use Wocky.DataCase, async: true

  alias Wocky.Push.Log

  @attrs [:user_id, :resource, :token, :response]

  test "required attributes" do
    changeset = Log.insert_changeset(%{})
    refute changeset.valid?

    for a <- @attrs do
      assert "can't be blank" in errors_on(changeset)[a]
    end
  end
end
