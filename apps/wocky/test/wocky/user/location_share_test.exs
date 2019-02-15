defmodule Wocky.User.LocationShareTest do
  use Wocky.WatcherHelper

  alias Wocky.{Repo, Roster, User}
  alias Wocky.Repo.{Factory, Timestamp}
  alias Wocky.Tasks.LocShareExpire
  alias Wocky.User.Notification

  setup do
    [u1, u2] = Factory.insert_list(2, :user)
    {:ok, user: u1, other_user: u2}
  end

  describe "creation/deletion" do
    test "creating/ending a location share generates a notification", ctx do
      Roster.befriend(ctx.user, ctx.other_user)

      {:ok, _} =
        User.start_sharing_location(
          ctx.other_user,
          ctx.user,
          Timestamp.shift(days: 1)
        )

      assert_eventually(Repo.get_by(Notification, user_id: ctx.user.id) != nil)
      other_id = ctx.other_user.id

      assert %Notification{
               type: :location_share,
               id: id,
               other_user_id: ^other_id
             } = Repo.get_by(Notification, user_id: ctx.user.id)

      Notification.delete(id, ctx.user)

      User.stop_sharing_location(ctx.other_user, ctx.user)

      assert_eventually(Repo.get_by(Notification, user_id: ctx.user.id) != nil)

      assert %Notification{type: :location_share_end, other_user_id: ^other_id} =
               Repo.get_by(Notification, user_id: ctx.user.id)
    end
  end

  describe "expiration" do
    setup ctx do
      Enum.map(
        1..5,
        fn _ ->
          Factory.insert(:user_location_share,
            shared_with: ctx.user,
            expires_at: Timestamp.shift(seconds: -1)
          )
        end
      )

      shares =
        Enum.map(
          1..5,
          fn _ ->
            Factory.insert(:user_location_share,
              shared_with: ctx.user,
              expires_at: Timestamp.shift(minutes: 1)
            )
          end
        )

      {:ok, shares: Enum.sort(shares)}
    end

    test "should remove all expired shares", ctx do
      LocShareExpire.expire_loc_shares()

      assert ctx.user |> User.get_location_sharers() |> Enum.sort() ==
               ctx.shares
    end
  end
end
