defmodule Wocky.Location.HandlerTest do
  use Wocky.DataCase, async: false

  alias Wocky.Location.Handler

  describe "get_handler/1" do
    test "both versions should get the same handler" do
      user = Factory.insert(:user)

      assert Handler.get_handler(user.id) == Handler.get_handler(user)
    end
  end
end
