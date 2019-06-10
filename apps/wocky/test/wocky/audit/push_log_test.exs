defmodule Wocky.Audit.PushLogTest do
  use Wocky.DataCase, async: true

  alias Wocky.Audit.PushLog

  @attrs [:user_id, :device, :token, :response]

  test "required attributes" do
    changeset = PushLog.insert_changeset(%{})
    refute changeset.valid?

    for a <- @attrs do
      assert "can't be blank" in errors_on(changeset)[a]
    end
  end
end
