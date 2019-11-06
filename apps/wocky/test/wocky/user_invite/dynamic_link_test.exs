defmodule Wocky.UserInvite.DynamicLinkTest do
  use Wocky.DataCase, async: false

  alias Wocky.UserInvite
  alias Wocky.UserInvite.DynamicLink
  alias Wocky.UserInvite.DynamicLink.Sandbox
  alias Wocky.UserInvite.InviteCode
  alias Wocky.Repo.Factory

  setup do
    user = Factory.insert(:user)
    {:ok, user: user}
  end

  describe "invitation_link/1" do
    test "should return a link with the invite code", %{user: user} do
      Sandbox.set_result(:ok)

      code = UserInvite.make_code(user)
      assert {:ok, link} = DynamicLink.invitation_link(code)

      invite = Repo.get_by(InviteCode, user_id: user.id)

      assert link =~ invite.code
    end

    test "should pass backend errors", %{user: user} do
      Sandbox.set_result({:error, :testing})

      assert {:error, :testing} = DynamicLink.invitation_link(user)
    end
  end
end
