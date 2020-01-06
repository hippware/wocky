defmodule Wocky.Callbacks.RelationshipTest do
  use Wocky.WatcherCase

  alias Wocky.Callbacks.Relationship, as: Callback
  alias Wocky.Contacts
  alias Wocky.Contacts.Share.Cache
  alias Wocky.Contacts.Share.CachedRelationship
  alias Wocky.Notifier.InBand
  alias Wocky.Notifier.InBand.Notification

  setup_all do
    Callback.register()
  end

  setup do
    [user, friend1, friend2] = Factory.insert_list(3, :user)

    Contacts.befriend(user, friend1)
    Contacts.befriend(user, friend2)

    {:ok, friendship1} = Contacts.update_sharing(user, friend1, :always)
    {:ok, friendship2} = Contacts.update_sharing(user, friend2, :disabled)

    {:ok,
     user: user,
     friend1: friend1,
     friend2: friend2,
     friendship1: friendship1,
     friendship2: friendship2}
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

      Contacts.update_sharing(ctx.user, ctx.friend1, :disabled)

      assert_eventually(in_band_notification_count(ctx.friend1) == 2)

      other_id = ctx.user.id

      assert [
               %Notification{
                 type: :location_share_end,
                 other_user_id: ^other_id
               }
             ] = ctx.friend1 |> InBand.user_query(nil, id) |> Repo.all()
    end

    test """
         ending a location share generates a notification for the sharing user
         """,
         ctx do
      Contacts.update_sharing(ctx.user, ctx.friend1, :disabled)

      assert_eventually(in_band_notification_count(ctx.user) == 1)

      other_id = ctx.friend1.id

      assert [
               %Notification{
                 type: :location_share_end_self,
                 other_user_id: ^other_id
               }
             ] = ctx.user |> InBand.user_query(nil, nil) |> Repo.all()
    end
  end

  describe "location share cache" do
    test "starting a share inserts it into the cache", ctx do
      assert_eventually(
        [CachedRelationship.new(ctx.friendship1)] == Cache.get(ctx.user.id)
      )
    end

    test "starting another share inserts it into the cache", ctx do
      {:ok, _} = Contacts.update_sharing(ctx.user, ctx.friend2, :always)

      assert_eventually(
        [ctx.friendship1, %{ctx.friendship2 | share_type: :always}]
        |> Enum.map(&CachedRelationship.new/1)
        |> Enum.sort() ==
          ctx.user.id |> Cache.get() |> Enum.sort()
      )
    end

    test "ending a share removes it from the cache", ctx do
      assert_eventually(
        [CachedRelationship.new(ctx.friendship1)] == Cache.get(ctx.user.id)
      )

      Contacts.stop_sharing_location(ctx.user)
      assert_eventually([] == Cache.get(ctx.user.id))
    end

    test "unfriending updates the cache", ctx do
      assert_eventually(
        [CachedRelationship.new(ctx.friendship1)] == Cache.get(ctx.user.id)
      )

      Contacts.unfriend(ctx.user, ctx.friend1)
      assert_eventually([] == Cache.get(ctx.user.id))
    end
  end

  defp in_band_notifications(user),
    do: user |> InBand.user_query(nil, nil) |> Repo.all()

  defp in_band_notification_count(user),
    do: user |> in_band_notifications() |> length()
end
