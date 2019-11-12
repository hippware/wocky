defmodule Wocky.UserInvite.UserInviteTest do
  use Wocky.DataCase, async: false

  import Mock

  alias Wocky.Account.User
  alias Wocky.Block
  alias Wocky.Friends
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.SMS.Sandbox, as: SMSSandbox
  alias Wocky.UserInvite
  alias Wocky.UserInvite.DynamicLink.Sandbox, as: DLSandbox
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
      phone = Factory.phone_number()

      assert {:ok, code} = UserInvite.make_code(ctx.user, phone, :always)

      assert invitation =
               Repo.get_by(InviteCode, user_id: ctx.user.id, code: code)

      assert invitation.phone_number == phone
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

  describe "send/3 errors" do
    test "should return phone number parsing errors", ctx do
      result = UserInvite.send("bad_number", :always, ctx.user)

      assert result.result == :could_not_parse_number
    end
  end

  describe "send/3 internal invitations" do
    test "should send internal invitation to existing user", ctx do
      other_user = Factory.insert(:user)
      result = UserInvite.send(other_user.phone_number, :always, ctx.user)

      assert result.result == :internal_invitation_sent

      assert {:invited, invitation} =
               Friends.get_relationship(ctx.user, other_user)

      assert invitation.share_type == :always
    end

    test "should normalise phone number when inviting existing user", ctx do
      other_user = Factory.insert(:user)
      {"+1", n} = String.split_at(other_user.phone_number, 2)

      result = UserInvite.send(n, :always, ctx.user)

      assert result.result == :internal_invitation_sent

      assert {:invited, invitation} =
               Friends.get_relationship(ctx.user, other_user)

      assert invitation.share_type == :always
    end

    test "should not send an invitation to an existing friend", ctx do
      other_user = Factory.insert(:user)
      :ok = Friends.befriend(ctx.user, other_user)

      result = UserInvite.send(other_user.phone_number, :always, ctx.user)

      assert result.result == :already_friends
    end

    test "should not send an invitation to self", ctx do
      result = UserInvite.send(ctx.user.phone_number, :always, ctx.user)

      assert result.result == :self
    end
  end

  describe "send/3 SMS invitations" do
    setup do
      SMSSandbox.set_result(:ok)
      DLSandbox.set_result(:ok)

      :ok
    end

    test "should send SMS invitation to new user", ctx do
      with_mock(SMSSandbox, [:passthrough], send: fn _, _ -> :ok end) do
        phone_number = Factory.phone_number()
        result = UserInvite.send(phone_number, :always, ctx.user)

        assert result.result == :external_invitation_sent
        assert Repo.get(User, ctx.user.id).smss_sent == 1

        assert_called(SMSSandbox.send(phone_number, :_))

        invitation =
          InviteCode
          |> where(phone_number: ^phone_number)
          |> Repo.one()

        assert invitation.share_type == :always
      end
    end

    test "should normalise phone number when sending SMS", ctx do
      with_mock(SMSSandbox, [:passthrough], send: fn _, _ -> :ok end) do
        n = Factory.phone_number()
        {"+1", n2} = String.split_at(n, 2)

        result = UserInvite.send(n2, :always, ctx.user)

        assert result.result == :external_invitation_sent
        assert Repo.get(User, ctx.user.id).smss_sent == 1

        assert_called(SMSSandbox.send(n, :_))
        assert not called(SMSSandbox.send(n2, :_))
      end
    end

    test "should return SMS errors", ctx do
      SMSSandbox.set_result({:error, "testing"})

      phone_number = Factory.phone_number()
      result = UserInvite.send(phone_number, :always, ctx.user)

      assert result.result == :sms_error
      assert result.error =~ "testing"
    end

    test "should return SMS errors with codes", ctx do
      SMSSandbox.set_result({:error, "testing", 100})

      phone_number = Factory.phone_number()
      result = UserInvite.send(phone_number, :always, ctx.user)

      assert result.result == :sms_error
      assert result.error =~ "testing"
      assert result.error =~ "100"
    end

    test "should return an error when link generation fails", ctx do
      DLSandbox.set_result({:error, "testing"})

      phone_number = Factory.phone_number()
      result = UserInvite.send(phone_number, :always, ctx.user)

      assert result.result == :sms_error
      assert result.error =~ "testing"
    end
  end

  # DEPRECATED
  describe "send_multi/2 errors" do
    test "should return phone number parsing errors", ctx do
      assert [result] = UserInvite.send_multi(["bad_number"], ctx.user)
      assert result.result == :could_not_parse_number
    end
  end

  describe "send_multi/2 internal invitations" do
    test "should send internal invitation to existing user", ctx do
      other_user = Factory.insert(:user)

      assert [result] =
               UserInvite.send_multi([other_user.phone_number], ctx.user)

      assert result.result == :internal_invitation_sent

      assert {:invited, invitation} =
               Friends.get_relationship(ctx.user, other_user)

      assert invitation.share_type == :disabled
    end

    test "should normalise phone number when inviting existing users", ctx do
      other_user = Factory.insert(:user)
      n = other_user.phone_number
      {"+1", n2} = String.split_at(n, 2)

      assert [r1, r2] = UserInvite.send_multi([n, n2], ctx.user)
      assert r1.result == :internal_invitation_sent
      assert r2.result == :internal_invitation_sent

      assert {:invited, invitation} =
               Friends.get_relationship(ctx.user, other_user)

      assert invitation.share_type == :disabled
    end

    test "should not send an invitation to an existing friend", ctx do
      other_user = Factory.insert(:user)
      :ok = Friends.befriend(ctx.user, other_user)

      assert [result] =
               UserInvite.send_multi([other_user.phone_number], ctx.user)

      assert result.result == :already_friends
    end

    test "should not send an invitation to self", ctx do
      assert [result] = UserInvite.send_multi([ctx.user.phone_number], ctx.user)

      assert result.result == :self
    end
  end

  describe "send_multi/2 SMS invitations" do
    setup do
      SMSSandbox.set_result(:ok)
      DLSandbox.set_result(:ok)

      :ok
    end

    test "should send SMS invitation to new user", ctx do
      with_mock(SMSSandbox, [:passthrough], send: fn _, _ -> :ok end) do
        phone_number = Factory.phone_number()

        assert [result] = UserInvite.send_multi([phone_number], ctx.user)
        assert result.result == :external_invitation_sent
        assert Repo.get(User, ctx.user.id).smss_sent == 1

        assert_called(SMSSandbox.send(phone_number, :_))

        invitation =
          InviteCode
          |> where(phone_number: ^phone_number)
          |> Repo.one()

        assert invitation.share_type == :disabled
      end
    end

    test "should normalise phone number when sending SMS", ctx do
      with_mock(SMSSandbox, [:passthrough], send: fn _, _ -> :ok end) do
        n = Factory.phone_number()
        {"+1", n2} = String.split_at(n, 2)

        assert [r1, r2] = UserInvite.send_multi([n, n2], ctx.user)
        assert r1.result == :external_invitation_sent
        assert r2.result == :external_invitation_sent
        assert Repo.get(User, ctx.user.id).smss_sent == 1

        assert_called(SMSSandbox.send(n, :_))
        assert not called(SMSSandbox.send(n2, :_))
      end
    end

    test "should return SMS errors", ctx do
      SMSSandbox.set_result({:error, "testing"})

      n = Factory.phone_number()
      {"+1", n2} = String.split_at(n, 2)

      results = UserInvite.send_multi([n, n2], ctx.user)

      assert Enum.any?(results, fn r ->
               r.phone_number == n &&
                 r.result == :sms_error &&
                 r.error =~ "testing"
             end)
    end

    test "should return SMS errors with codes", ctx do
      SMSSandbox.set_result({:error, "testing", 100})

      phone_number = Factory.phone_number()

      assert [result] = UserInvite.send_multi([phone_number], ctx.user)
      assert result.result == :sms_error
      assert result.error =~ "testing"
      assert result.error =~ "100"
    end

    test "should return an error when link generation fails", ctx do
      DLSandbox.set_result({:error, "testing"})

      phone_number = Factory.phone_number()

      assert [result] = UserInvite.send_multi([phone_number], ctx.user)
      assert result.result == :sms_error
      assert result.error =~ "testing"
    end
  end

  describe "send_multi/2 multiple invitation types" do
    setup do
      SMSSandbox.set_result(:ok)
      DLSandbox.set_result(:ok)

      :ok
    end

    test "should eliminate duplicate numbers", ctx do
      other_user = Factory.insert(:user)
      number = other_user.phone_number
      results = UserInvite.send_multi([number, number], ctx.user)

      assert length(results) == 1
    end

    test "should work for multiple invitation types in one request", ctx do
      phone_number = Factory.phone_number()
      other_user = Factory.insert(:user)
      friend = Factory.insert(:user)
      {"+1", denormalised} = String.split_at(phone_number, 2)

      :ok = Friends.befriend(ctx.user, friend)

      numbers = [
        phone_number,
        other_user.phone_number,
        phone_number,
        friend.phone_number,
        ctx.user.phone_number,
        denormalised,
        "bad_number"
      ]

      results = UserInvite.send_multi(numbers, ctx.user)

      assert length(results) == 6

      assert Enum.any?(results, fn r ->
               r.phone_number == phone_number &&
                 r.result == :external_invitation_sent
             end)

      assert Enum.any?(results, fn r ->
               r.phone_number == denormalised &&
                 r.result == :external_invitation_sent
             end)

      assert Enum.any?(results, fn r ->
               r.phone_number == other_user.phone_number &&
                 r.result == :internal_invitation_sent
             end)

      assert Enum.any?(results, fn r ->
               r.phone_number == friend.phone_number &&
                 r.result == :already_friends
             end)

      assert Enum.any?(results, fn r ->
               r.phone_number == ctx.user.phone_number &&
                 r.result == :self
             end)

      assert Enum.any?(results, fn r ->
               r.phone_number == "bad_number" &&
                 r.result == :could_not_parse_number
             end)
    end
  end
end
