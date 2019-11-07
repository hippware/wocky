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
    test "should generate a string code", ctx do
      assert {:ok, code} = UserInvite.make_code(ctx.user)
      assert is_binary(code)
    end

    test "should store the invitation in the database", ctx do
      assert {:ok, code} = UserInvite.make_code(ctx.user, "5551112222", :always)

      assert invitation =
               Repo.get_by(InviteCode, user_id: ctx.user.id, code: code)

      assert invitation.phone_number == "+15551112222"
      assert invitation.share_type == :always
    end

    test "should return an error with bad phone number", ctx do
      assert {:error, changeset} =
               UserInvite.make_code(ctx.user, "bad", :always)

      assert errors_on(changeset).phone_number == ["invalid"]
    end
  end

  describe "redeem_code/2" do
    setup %{user: user} do
      redeemer = Factory.insert(:user)

      {:ok, code} = UserInvite.make_code(user, redeemer.phone_number, :always)

      {:ok, code: code, redeemer: redeemer}
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
