defmodule Wocky.UserInvite.UserInviteTest do
  use Wocky.DataCase, async: true

  alias Wocky.Block
  alias Wocky.Friends
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.UserInvite
  alias Wocky.UserInvite.InviteCode

  setup do
    {:ok, user: Factory.insert(:user)}
  end

  describe "make_code/1" do
    setup %{user: user} do
      code = UserInvite.make_code(user)
      {:ok, code: code}
    end

    test "it should generate a string code", ctx do
      assert is_binary(ctx.code)
    end

    test "it should store the code in the database", ctx do
      assert Repo.get_by(InviteCode, user_id: ctx.user.id, code: ctx.code)
    end
  end

  describe "redeem_code/2" do
    setup %{user: user} do
      code = UserInvite.make_code(user)
      user = Factory.insert(:user)
      {:ok, code: code, redeemer: user}
    end

    test "it should friend the current user and the owner of the code", ctx do
      assert UserInvite.redeem_code(ctx.redeemer, ctx.code)
      assert Friends.friend?(ctx.user, ctx.redeemer)
    end

    test "it should return true if the redeemer created the code", ctx do
      assert UserInvite.redeem_code(ctx.user, ctx.code)
    end

    test "it should return false if the code is bad", ctx do
      bad_code = InviteCode.generate()
      refute UserInvite.redeem_code(ctx.redeemer, bad_code)
    end

    test "it should return false if the code is expired", ctx do
      invitation = Repo.get_by(InviteCode, code: ctx.code)
      assert invitation

      ts = Timex.shift(invitation.created_at, days: -31)

      invitation
      |> Ecto.Changeset.change(created_at: ts)
      |> Repo.update!()

      refute UserInvite.redeem_code(ctx.redeemer, ctx.code)
    end

    test "it should return false if the redeemer is blocked", ctx do
      Block.block(ctx.user, ctx.redeemer)
      refute UserInvite.redeem_code(ctx.redeemer, ctx.code)
    end
  end
end
