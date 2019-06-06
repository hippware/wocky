defmodule Wocky.Tasks.LocShareExpireTest do
  use Wocky.DataCase

  alias Wocky.Location
  alias Wocky.Repo.Factory
  alias Wocky.Repo.Timestamp
  alias Wocky.Tasks.LocShareExpire

  setup do
    user = Factory.insert(:user)

    Enum.map(
      1..5,
      fn _ ->
        Factory.insert(:user_location_share,
          shared_with: user,
          expires_at: Timestamp.shift(seconds: -1)
        )
      end
    )

    shares =
      Enum.map(
        1..5,
        fn _ ->
          Factory.insert(:user_location_share,
            shared_with: user,
            expires_at: Timestamp.shift(minutes: 1)
          )
        end
      )

    {:ok, user: user, shares: Enum.sort(shares)}
  end

  test "should remove all expired shares", ctx do
    LocShareExpire.run()

    assert ctx.user |> Location.get_location_sharers() |> Enum.sort() ==
             ctx.shares
  end
end
