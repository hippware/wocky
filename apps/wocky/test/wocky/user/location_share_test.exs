defmodule Wocky.User.LocationShareTest do
  use Wocky.WatcherHelper

  alias Wocky.{Repo, Roster, User}
  alias Wocky.Repo.{Factory, Timestamp}
  alias Wocky.User.Notification

  setup do
    [u1, u2] = Factory.insert_list(2, :user)
    {:ok, user: u1, other_user: u2}
  end

  test "creating a location share generates a notification", ctx do
    Roster.befriend(ctx.user, ctx.other_user)

    {:ok, _} =
      User.start_sharing_location(
        ctx.other_user,
        ctx.user,
        Timestamp.shift(days: 1)
      )

    assert_eventually(Repo.get_by(Notification, user_id: ctx.user.id) != nil)
    other_id = ctx.other_user.id

    assert %Notification{type: :location_share, other_user_id: ^other_id} =
             Repo.get_by(Notification, user_id: ctx.user.id)
  end
end
