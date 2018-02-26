defmodule Wocky.HomeStream.PrepopSpec do
  use ESpec, async: true
  use ModelHelpers

  alias Timex.Duration
  alias Wocky.HomeStream
  alias Wocky.HomeStream.Prepop
  alias Wocky.InitialContact
  alias Wocky.Repo
  alias Wocky.RosterItem

  @num_items 10
  @differing_prepop_fields [:user, :user_id, :id]

  before do
    # This user is inserted by the db migrations, however we'd rather have this
    # test also work properly on a completely empty DB
    Factory.build(:user, handle: Prepop.handle())
    |> Repo.insert(on_conflict: :nothing)

    {:ok, source_user: Prepop.user()}
  end

  describe "add_source/1" do
    before do
      user = Factory.insert(:user)
      result = Prepop.add_source(user.handle)
      {:ok, user: user, result: result}
    end

    it "should return ok" do
      shared.result |> should(eq :ok)
    end

    it "should make the prepop user follow the specified user" do
      RosterItem.relationship(shared.source_user.id, shared.user.id)
      |> should(eq :follower)
    end

    it "should make the user an initial contact followee" do
      ic = InitialContact.get() |> hd
      ic |> should(have user_id: shared.user.id)
      ic |> should(have type: :followee)
    end
  end

  describe "prepopulate/2" do
    before do
      now = DateTime.utc_now()

      items =
        for i <- 1..@num_items do
          time = Timex.shift(now, seconds: -(i * 5))

          Factory.insert(:home_stream_item, %{
            user: shared.source_user,
            created_at: time,
            updated_at: time
          })
          |> Repo.preload(:reference_bot)
        end

      target_user = Factory.insert(:user)

      {:ok,
       items:
         Enum.map(Enum.reverse(items), &Map.drop(&1, @differing_prepop_fields)),
       target_user: target_user}
    end

    it "should copy items for the specified time period" do
      Prepop.prepopulate(shared.target_user.id, [
        period: Duration.from_days(10),
        min: 0
      ])
      |> should(eq :ok)

      shared.target_user.id
      |> HomeStream.get()
      |> Enum.map(&Map.drop(&1, @differing_prepop_fields))
      |> should(eq shared.items)
    end

    it "should not copy items if a zero period is given" do
      Prepop.prepopulate(shared.target_user.id, [
        period: Duration.from_seconds(0),
        min: 0
      ])
      |> should(eq :ok)

      shared.target_user.id
      |> HomeStream.get()
      |> should(eq [])
    end

    it "should copy only items in the time period" do
      pivot = Enum.at(shared.items, 5)
      period = Timex.diff(DateTime.utc_now(), pivot.created_at(), :duration)

      Prepop.prepopulate(shared.target_user.id, period: period, min: 0)
      |> should(eq :ok)

      shared.target_user.id
      |> HomeStream.get()
      |> Enum.map(&Map.drop(&1, @differing_prepop_fields))
      |> should(eq Enum.slice(shared.items, 6..(@num_items - 1)))
    end

    it "should copy the minimum requested items even if
        outside the time period" do
      Prepop.prepopulate(shared.target_user.id, [
        period: Timex.Duration.from_seconds(1),
        min: 8
      ])
      |> should(eq :ok)

      shared.target_user.id
      |> HomeStream.get()
      |> Enum.map(&Map.drop(&1, @differing_prepop_fields))
      |> should(eq Enum.slice(shared.items, (@num_items - 8)..(@num_items - 1)))
    end
  end
end
