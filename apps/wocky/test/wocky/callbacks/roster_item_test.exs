defmodule Wocky.Callbacks.RosterItemTest do
  use Wocky.WatcherCase

  alias Wocky.Callbacks.RosterItem, as: Callback
  alias Wocky.Notifier.InBand.Notification
  alias Wocky.Repo.Factory
  alias Wocky.Roster
  alias Wocky.Roster.Share.Cache

  setup_all do
    Callback.register()
  end

  setup do
    [user, friend1, friend2] = Factory.insert_list(3, :user)

    Roster.befriend(user, friend1, false)
    Roster.befriend(user, friend2, false)

    {:ok, _} = Roster.start_sharing_location(user, friend1)

    {:ok, user: user, friend1: friend1, friend2: friend2}
  end

  describe "notifications" do
    test "creating a location share generates a notification", ctx do
      assert_eventually(in_band_notification_count(ctx.friend1) == 1)
      other_id = ctx.user.id

      assert %Notification{
               type: :location_share,
               id: id,
               other_user_id: ^other_id
             } = Repo.get_by(Notification, user_id: ctx.friend1.id)
    end

    test """
         ending a location share generates a notification for the shared-to user
         """,
         ctx do
      assert_eventually(in_band_notification_count(ctx.friend1) == 1)
      %Notification{id: id} = Repo.get_by(Notification, user_id: ctx.friend1.id)

      Roster.stop_sharing_location(ctx.user, ctx.friend1)

      assert_eventually(in_band_notification_count(ctx.friend1) == 2)

      other_id = ctx.user.id

      assert [
               %Notification{
                 type: :location_share_end,
                 other_user_id: ^other_id
               }
             ] = ctx.friend1 |> Notification.user_query(nil, id) |> Repo.all()
    end

    test """
         ending a location share generates a notification for the sharing user
         """,
         ctx do
      Roster.stop_sharing_location(ctx.user, ctx.friend1)

      assert_eventually(in_band_notification_count(ctx.user) == 1)

      other_id = ctx.friend1.id

      assert [
               %Notification{
                 type: :location_share_end_self,
                 other_user_id: ^other_id
               }
             ] = ctx.user |> Notification.user_query(nil, nil) |> Repo.all()
    end
  end

  describe "location share cache" do
    test "starting a share inserts it into the cache", ctx do
      assert_eventually([ctx.friend1.id] == Cache.get(ctx.user.id))
    end

    test "starting another share inserts it into the cache", ctx do
      {:ok, _} = Roster.start_sharing_location(ctx.user, ctx.friend2)

      assert_eventually(
        [ctx.friend1.id, ctx.friend2.id] |> Enum.sort() ==
          ctx.user.id |> Cache.get() |> Enum.sort()
      )
    end

    test "ending a share removes it from the cache", ctx do
      assert_eventually([ctx.friend1.id] == Cache.get(ctx.user.id))

      Roster.stop_sharing_location(ctx.user)
      assert_eventually([] == Cache.get(ctx.user.id))
    end

    test "unfriending updates the cache", ctx do
      assert_eventually([ctx.friend1.id] == Cache.get(ctx.user.id))

      Roster.unfriend(ctx.user, ctx.friend1)
      assert_eventually([] == Cache.get(ctx.user.id))
    end
  end

  defp in_band_notifications(user),
    do: user |> Notification.user_query(nil, nil) |> Repo.all()

  defp in_band_notification_count(user),
    do: user |> in_band_notifications() |> length()
end
