defmodule Wocky.User.UserTest do
  use Wocky.DataCase, async: false

  import Mock

  alias Wocky.Repo.Factory
  alias Wocky.User

  describe "user deletion" do
    setup_with_mocks([
      {FirebaseAdminEx.Auth, [], [delete_user: fn _ -> :ok end]}
    ]) do
      :ok
    end

    test "should delete user's firebase account if they have one" do
      user = Factory.insert(:user, provider: "firebase")
      assert User.delete(user.id) == :ok

      assert_called FirebaseAdminEx.Auth.delete_user(user.external_id)
    end

    test "should not delete user's firebase account if they don't have one" do
      user = Factory.insert(:user)
      assert User.delete(user.id) == :ok

      refute called FirebaseAdminEx.Auth.delete_user(user.external_id)
    end
  end
end

