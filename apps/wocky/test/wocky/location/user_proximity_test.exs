defmodule Wocky.Location.UserProximityTest do
  use Wocky.WatcherCase

  alias Faker.Address
  alias Wocky.Callbacks.UserProximity, as: Callback
  alias Wocky.Factory, as: BaseFactory
  alias Wocky.Location
  alias Wocky.Location.Handler
  alias Wocky.Location.Supervisor, as: HandlerSup
  alias Wocky.Location.UserProximity.Subscription
  alias Wocky.Notifier.InBand.Notification
  alias Wocky.Repo

  setup_all do
    Callback.register()
  end

  setup do
    [u1, u2] = Factory.insert_list(2, :user)

    prox1 = Factory.insert(:user_proximity, user: u1, target: u2, cooldown: 500)

    prox2 = Factory.insert(:user_proximity, user: u2, target: u1, range: 10_000)

    on_exit(fn ->
      HandlerSup
      |> Supervisor.which_children()
      |> Enum.each(fn {_, pid, _, _} -> :ok = GenServer.stop(pid) end)
    end)

    {:ok, user1: u1, user2: u2, prox1: prox1, prox2: prox2}
  end

  describe "Notification generation" do
    test "Setting adjacent locations will notify both users", ctx do
      lat = Address.latitude()
      lon = Address.longitude()

      {:ok, _} =
        Location.set_user_location(
          ctx.user1,
          BaseFactory.build(:user_location,
            user_id: ctx.user1.id,
            lat: lat,
            lon: lon
          )
        )

      {:ok, _} =
        Location.set_user_location(
          ctx.user2,
          BaseFactory.build(:user_location,
            user_id: ctx.user2.id,
            lat: lat,
            lon: lon
          )
        )

      assert_eventually(
        (
          n = Repo.get_by(Notification, user_id: ctx.user1.id)
          is_map(n) && n.type == :user_proximity
        )
      )

      assert_eventually(
        (
          n = Repo.get_by(Notification, user_id: ctx.user2.id)
          is_map(n) && n.type == :user_proximity
        )
      )
    end

    test "repeated adjacent locations won't notify inside the cooldown", ctx do
      lat = Address.latitude()
      lon = Address.longitude()

      {:ok, _} =
        Location.set_user_location(
          ctx.user1,
          BaseFactory.build(:user_location,
            user_id: ctx.user1.id,
            lat: lat,
            lon: lon
          )
        )

      {:ok, _} =
        Location.set_user_location(
          ctx.user2,
          BaseFactory.build(:user_location,
            user_id: ctx.user2.id,
            lat: lat,
            lon: lon
          )
        )

      Process.sleep(500)

      {:ok, _} =
        Location.set_user_location(
          ctx.user2,
          BaseFactory.build(:user_location,
            user_id: ctx.user2.id,
            lat: lat,
            lon: lon
          )
        )

      assert_eventually(
        (
          n =
            Notification
            |> where(user_id: ^ctx.user1.id)
            |> Repo.all()

          length(n) == 2 && Enum.all?(n, fn x -> x.type == :user_proximity end)
        )
      )

      # Still only one notification on the user with the longer cooldown
      assert_eventually(
        (
          n = Repo.get_by(Notification, user_id: ctx.user2.id)
          is_map(n) && n.type == :user_proximity
        )
      )
    end

    test "should only notify when within range", ctx do
      lat = Address.latitude()
      lon = Address.longitude()

      {:ok, _} =
        Location.set_user_location(
          ctx.user2,
          BaseFactory.build(:user_location,
            user_id: ctx.user2.id,
            lat: lat,
            lon: lon
          )
        )

      # Adds ~1.1km to distance, moving it outside user2's range
      lat2 = lat + 0.01

      {:ok, _} =
        Location.set_user_location(
          ctx.user1,
          BaseFactory.build(:user_location,
            user_id: ctx.user1.id,
            lat: lat2,
            lon: lon
          )
        )

      assert_eventually(
        (
          n = Repo.get_by(Notification, user_id: ctx.user2.id)
          is_map(n) && n.type == :user_proximity
        )
      )

      refute Repo.get_by(Notification, user_id: ctx.user1.id)
    end
  end

  describe "Cache management" do
    setup ctx do
      # Warm up the handler caches
      h1 = Handler.get_handler(ctx.user1)
      h2 = Handler.get_handler(ctx.user2)
      {:ok, h1: h1, h2: h2}
    end

    test "should cache existing shares", ctx do
      assert matches?(:sys.get_state(ctx.h1).proximity_subscriptions, [
               ctx.prox1
             ])

      assert matches?(:sys.get_state(ctx.h1).proximity_subscribers, [ctx.prox2])

      assert matches?(:sys.get_state(ctx.h2).proximity_subscriptions, [
               ctx.prox2
             ])

      assert matches?(:sys.get_state(ctx.h2).proximity_subscribers, [ctx.prox1])
    end

    test "should update both users when deleted", ctx do
      Repo.delete(ctx.prox1)

      assert_eventually(:sys.get_state(ctx.h1).proximity_subscriptions == [])
      assert_eventually(:sys.get_state(ctx.h2).proximity_subscribers == [])
    end

    test "should update both users when updated", ctx do
      ctx.prox1
      |> Subscription.changeset(%{cooldown: 5000})
      |> Repo.update()

      prox1 = %{ctx.prox1 | cooldown: 5000}

      Repo.get_by(Subscription, user_id: ctx.prox1.user_id)

      assert_eventually(
        matches?(:sys.get_state(ctx.h1).proximity_subscriptions, [prox1])
      )

      assert_eventually(
        matches?(:sys.get_state(ctx.h2).proximity_subscribers, [prox1])
      )
    end

    test "should add a new subscription when it's created", ctx do
      user3 = Factory.insert(:user)
      h3 = Handler.get_handler(user3)

      prox3 = Factory.insert(:user_proximity, user: user3, target: ctx.user1)

      assert_eventually(
        matches?(:sys.get_state(ctx.h1).proximity_subscribers, [
          ctx.prox2,
          prox3
        ])
      )

      assert_eventually(matches?(:sys.get_state(h3).proximity_subscribers, []))

      assert_eventually(
        matches?(:sys.get_state(h3).proximity_subscriptions, [prox3])
      )
    end
  end

  defp matches?(l1, l2), do: clean_and_sort(l1) == clean_and_sort(l2)

  defp clean_and_sort(l),
    do:
      l
      |> Enum.map(&Map.drop(&1, [:__meta__, :user, :target, :updated_at]))
      |> Enum.sort()
end
