defmodule Wocky.RSMHelperTest do
  use Wocky.DataCase, async: true
  use Wocky.RSMHelper

  import Ecto.Query

  alias Wocky.Bot
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID

  require Record

  @count 20

  setup do
    user = Factory.insert(:user)
    Factory.insert_list(@count, :bot, user: user)
    query = where(Bot, user_id: ^user.id)

    bots =
      Bot
      |> where(user_id: ^user.id)
      |> order_by(asc: :created_at)
      |> Repo.all()

    {:ok, user: user, bots: bots, query: query}
  end

  test "simple max query", ctx do
    ctx = setup_query(ctx, rsm_in(max: 2))

    assert ctx.records == Enum.slice(ctx.bots, 0..1)
    assert rsm_out(ctx.rsm_out, :index) == 0
    assert rsm_out(ctx.rsm_out, :count) == @count
    assert rsm_out(ctx.rsm_out, :first) == hd(ctx.bots).id
    assert rsm_out(ctx.rsm_out, :last) == Enum.at(ctx.bots, 1).id
  end

  test "simple index query", ctx do
    ctx = setup_query(ctx, rsm_in(index: 2))

    assert ctx.records == Enum.slice(ctx.bots, 2..(@count - 1))
    assert rsm_out(ctx.rsm_out, :index) == 2
    assert rsm_out(ctx.rsm_out, :count) == @count
    assert rsm_out(ctx.rsm_out, :first) == Enum.at(ctx.bots, 2).id
    assert rsm_out(ctx.rsm_out, :last) == Enum.at(ctx.bots, -1).id
  end

  test "out of range index query", ctx do
    ctx = setup_query(ctx, rsm_in(index: 500))

    assert ctx.records == []
    assert rsm_out(ctx.rsm_out, :index) == :undefined
    assert rsm_out(ctx.rsm_out, :count) == @count
    assert rsm_out(ctx.rsm_out, :first) == :undefined
    assert rsm_out(ctx.rsm_out, :last) == :undefined
  end

  test "simple end of set query", ctx do
    ctx = setup_query(ctx, rsm_in(direction: :before, max: 2))

    assert ctx.records == Enum.slice(ctx.bots, (@count - 2)..(@count - 1))
    assert rsm_out(ctx.rsm_out, :index) == @count - 2
    assert rsm_out(ctx.rsm_out, :count) == @count
    assert rsm_out(ctx.rsm_out, :first) == Enum.at(ctx.bots, -2).id
    assert rsm_out(ctx.rsm_out, :last) == Enum.at(ctx.bots, -1).id
  end

  test "simple ID query", ctx do
    key = Enum.at(ctx.bots, 10).id
    ctx = setup_query(ctx, rsm_in(id: key, max: 3))

    assert ctx.records == Enum.slice(ctx.bots, 11..13)
    assert rsm_out(ctx.rsm_out, :index) == 11
    assert rsm_out(ctx.rsm_out, :count) == @count
    assert rsm_out(ctx.rsm_out, :first) == Enum.at(ctx.bots, 11).id
    assert rsm_out(ctx.rsm_out, :last) == Enum.at(ctx.bots, 13).id
  end

  test "clipped setup ID query", ctx do
    key = Enum.at(ctx.bots, 3).id
    ctx = setup_query(ctx, rsm_in(id: key, max: 5, direction: :before))

    assert ctx.records == Enum.slice(ctx.bots, 0..2)
    assert rsm_out(ctx.rsm_out, :index) == 0
    assert rsm_out(ctx.rsm_out, :count) == @count
    assert rsm_out(ctx.rsm_out, :first) == Enum.at(ctx.bots, 0).id
    assert rsm_out(ctx.rsm_out, :last) == Enum.at(ctx.bots, 2).id
  end

  test "clipped ID+index query (should drop index)", ctx do
    key = Enum.at(ctx.bots, 17).id
    ctx = setup_query(ctx, rsm_in(id: key, index: 3, max: 3))

    assert ctx.records == Enum.slice(ctx.bots, 18..19)
    assert rsm_out(ctx.rsm_out, :index) == 18
    assert rsm_out(ctx.rsm_out, :count) == @count
    assert rsm_out(ctx.rsm_out, :first) == Enum.at(ctx.bots, 18).id
    assert rsm_out(ctx.rsm_out, :last) == Enum.at(ctx.bots, 19).id
  end

  test "Non-existant ID - should return an empty set", ctx do
    ctx = setup_query(ctx, rsm_in(id: ID.new()))

    assert ctx.records == []
    assert rsm_out(ctx.rsm_out, :index) == :undefined
    assert rsm_out(ctx.rsm_out, :count) == @count
    assert rsm_out(ctx.rsm_out, :first) == :undefined
    assert rsm_out(ctx.rsm_out, :last) == :undefined
  end

  test "simple reverse query", ctx do
    ctx = setup_query(ctx, rsm_in(reverse: true, max: 4))

    assert ctx.records == Enum.reverse(Enum.slice(ctx.bots, 0..3))
    assert rsm_out(ctx.rsm_out, :index) == 0
    assert rsm_out(ctx.rsm_out, :count) == @count
    assert rsm_out(ctx.rsm_out, :first) == hd(ctx.bots).id
    assert rsm_out(ctx.rsm_out, :last) == Enum.at(ctx.bots, 3).id
  end

  test "simple reverse sort order", ctx do
    {records, rsm_out} =
      RSMHelper.rsm_query(
        rsm_in(max: 10),
        ctx.query,
        :id,
        {:desc, :created_at}
      )

    assert records ==
             Enum.reverse(Enum.slice(ctx.bots, (@count - 10)..(@count - 1)))

    assert rsm_out(rsm_out, :index) == 0
    assert rsm_out(rsm_out, :count) == @count
    assert rsm_out(rsm_out, :first) == Enum.at(ctx.bots, -1).id
    assert rsm_out(rsm_out, :last) == Enum.at(ctx.bots, 10).id
  end

  test "empty result set" do
    id = ID.new()
    query = where(Bot, user_id: ^id)

    {records, rsm_out} =
      RSMHelper.rsm_query(rsm_in(max: 20), query, :id, {:asc, :created_at})

    assert records == []
    assert rsm_out(rsm_out, :index) == :undefined
    assert rsm_out(rsm_out, :count) == 0
    assert rsm_out(rsm_out, :first) == :undefined
    assert rsm_out(rsm_out, :last) == :undefined
  end

  test "when the key field is a DateTime", ctx do
    {_records, rsm_out} =
      RSMHelper.rsm_query(
        rsm_in(max: 20),
        ctx.query,
        :created_at,
        {:asc, :created_at}
      )

    assert rsm_out |> rsm_out(:first) |> is_binary()
    assert rsm_out |> rsm_out(:last) |> is_binary()
  end

  test "when the key field is a float", ctx do
    {_records, rsm_out} =
      RSMHelper.rsm_query(
        rsm_in(max: 20),
        ctx.query,
        :radius,
        {:asc, :created_at}
      )

    assert rsm_out |> rsm_out(:first) |> is_binary()
    assert rsm_out |> rsm_out(:last) |> is_binary()
  end

  test "can be composed with more complex queries" do
    user1 = Factory.insert(:user)
    user2 = Factory.insert(:user)

    bots =
      1..5
      |> Enum.map(fn _ ->
        Factory.insert(:bot, user: user2)
        bot = Factory.insert(:bot, user: user1)
        bot.id
      end)

    query =
      Bot
      |> where(user_id: ^user1.id)
      |> Bot.is_visible_query(user1)

    {records, rsm_out} =
      RSMHelper.rsm_query(
        rsm_in(id: Enum.at(bots, 2)),
        query,
        :id,
        {:asc, :created_at}
      )

    records = Enum.map(records, &Map.get(&1, :id))

    assert records == Enum.slice(bots, 3, 2)
    assert rsm_out(rsm_out, :index) == 3
    assert rsm_out(rsm_out, :count) == 5
    assert rsm_out(rsm_out, :first) == Enum.at(bots, 3)
    assert rsm_out(rsm_out, :last) == Enum.at(bots, -1)
  end

  defp setup_query(ctx, rsm_in) do
    {records, rsm_out} =
      RSMHelper.rsm_query(rsm_in, ctx.query, :id, {:asc, :created_at})

    Map.merge(ctx, %{records: records, rsm_out: rsm_out})
  end
end
