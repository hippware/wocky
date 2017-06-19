defmodule Wocky.RSMHelperSpec do
  use ESpec, async: true
  use Wocky.Repo.Model
  use Wocky.RSMHelper

  require Record

  alias Wocky.Bot
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID

  @count 20

  before do
    user = Factory.insert(:user)
    Factory.insert_list(@count, :bot, user: user)
    query = where(Bot, [user_id: ^user.id])

    bots =
      Bot
      |> where(user_id: ^user.id)
      |> order_by(asc: :created_at)
      |> Repo.all

    {:ok,
      user: user,
      bots: bots,
      query: query}
  end

  describe "rsm_query/4" do
    context "simple max query" do
      before do
        setup_query(shared, rsm_in(max: 2))
      end

      it do: shared.records |> should(eq Enum.slice(shared.bots, 0..1))
      it do: rsm_out(shared.rsm_out, :index) |> should(eq 0)
      it do: rsm_out(shared.rsm_out, :count) |> should(eq @count)
      it do: rsm_out(shared.rsm_out, :first) |> should(eq hd(shared.bots).id)
      it do: rsm_out(shared.rsm_out, :last) |>
             should(eq Enum.at(shared.bots, 1).id)
    end

    context "simple index query" do
      before do
        setup_query(shared, rsm_in(index: 2))
      end

      it do: shared.records |> should(eq Enum.slice(shared.bots, 2..@count-1))
      it do: rsm_out(shared.rsm_out, :index) |> should(eq 2)
      it do: rsm_out(shared.rsm_out, :count) |> should(eq @count)
      it do: rsm_out(shared.rsm_out, :first) |>
             should(eq Enum.at(shared.bots, 2).id)
      it do: rsm_out(shared.rsm_out, :last) |>
             should(eq Enum.at(shared.bots, -1).id)
    end

    context "simple end of set query" do
      before do
        setup_query(shared, rsm_in(direction: :before, max: 2))
      end

      it do: shared.records |>
             should(eq Enum.slice(shared.bots, @count-2..@count-1))
      it do: rsm_out(shared.rsm_out, :index) |> should(eq @count - 2)
      it do: rsm_out(shared.rsm_out, :count) |> should(eq @count)
      it do: rsm_out(shared.rsm_out, :first) |>
             should(eq Enum.at(shared.bots, -2).id)
      it do: rsm_out(shared.rsm_out, :last) |>
             should(eq Enum.at(shared.bots, -1).id)
    end

    context "simple ID query" do
      before do
        key = Enum.at(shared.bots, 10).id
        setup_query(shared, rsm_in(id: key, max: 3))
      end

      it do: shared.records |> should(eq Enum.slice(shared.bots, 11..13))
      it do: rsm_out(shared.rsm_out, :index) |> should(eq 11)
      it do: rsm_out(shared.rsm_out, :count) |> should(eq @count)
      it do: rsm_out(shared.rsm_out, :first) |>
             should(eq Enum.at(shared.bots, 11).id)
      it do: rsm_out(shared.rsm_out, :last) |>
             should(eq Enum.at(shared.bots, 13).id)
    end

    context "clipped before ID query" do
      before do
        key = Enum.at(shared.bots, 3).id
        setup_query(shared, rsm_in(id: key, max: 5, direction: :before))
      end

      it do: shared.records |> should(eq Enum.slice(shared.bots, 0..2))
      it do: rsm_out(shared.rsm_out, :index) |> should(eq 0)
      it do: rsm_out(shared.rsm_out, :count) |> should(eq @count)
      it do: rsm_out(shared.rsm_out, :first) |>
             should(eq Enum.at(shared.bots, 0).id)
      it do: rsm_out(shared.rsm_out, :last) |>
             should(eq Enum.at(shared.bots, 2).id)
    end

    context "clipped ID+index query (should drop index)" do
      before do
        key = Enum.at(shared.bots, 17).id
        setup_query(shared, rsm_in(id: key, index: 3, max: 3))
      end

      it do: shared.records |> should(eq Enum.slice(shared.bots, 18..19))
      it do: rsm_out(shared.rsm_out, :index) |> should(eq 18)
      it do: rsm_out(shared.rsm_out, :count) |> should(eq @count)
      it do: rsm_out(shared.rsm_out, :first) |>
             should(eq Enum.at(shared.bots, 18).id)
      it do: rsm_out(shared.rsm_out, :last) |>
             should(eq Enum.at(shared.bots, 19).id)
    end

    context "simple reverse query" do
      before do
        setup_query(shared, rsm_in(reverse: true, max: 4))
      end

      it do: shared.records |>
             should(eq Enum.reverse(Enum.slice(shared.bots, 0..3)))
      it do: rsm_out(shared.rsm_out, :index) |> should(eq 0)
      it do: rsm_out(shared.rsm_out, :count) |> should(eq @count)
      it do: rsm_out(shared.rsm_out, :first) |> should(eq hd(shared.bots).id)
      it do: rsm_out(shared.rsm_out, :last) |>
             should(eq Enum.at(shared.bots, 3).id)
    end

    context "simple reverse sort order" do
      before do
        {records, rsm_out} = RSMHelper.rsm_query(rsm_in(max: 10), shared.query,
                                                 :id, {:desc, :created_at})
        {:ok, records: records, rsm_out: rsm_out}
      end

      it do: shared.records |>
             should(eq Enum.reverse(
               Enum.slice(shared.bots, (@count - 10)..@count-1)))
      it do: rsm_out(shared.rsm_out, :index) |> should(eq 0)
      it do: rsm_out(shared.rsm_out, :count) |> should(eq @count)
      it do: rsm_out(shared.rsm_out, :first) |>
             should(eq Enum.at(shared.bots, @count-1).id)
      it do: rsm_out(shared.rsm_out, :last) |>
             should(eq Enum.at(shared.bots, 10).id)
    end

    context "empty result set" do
      before do
        id = ID.new
        query = where(Bot, [user_id: ^id])
        {records, rsm_out} = RSMHelper.rsm_query(rsm_in(max: 20), query,
                                                 :id, {:asc, :created_at})
        {:ok, records: records, rsm_out: rsm_out}
      end

      it do: shared.records |> should(eq [])
      it do: rsm_out(shared.rsm_out, :index) |> should(eq :undefined)
      it do: rsm_out(shared.rsm_out, :count) |> should(eq 0)
      it do: rsm_out(shared.rsm_out, :first) |> should(eq :undefined)
      it do: rsm_out(shared.rsm_out, :last) |> should(eq :undefined)
    end

    context "when the key field is a DateTime" do
      before do
        {records, rsm_out} = RSMHelper.rsm_query(rsm_in(max: 20), shared.query,
                                                 :created_at,
                                                 {:asc, :created_at})
        {:ok, records: records, rsm_out: rsm_out}
      end

      it do: rsm_out(shared.rsm_out, :first) |> should(be_binary())
      it do: rsm_out(shared.rsm_out, :last) |> should(be_binary())
    end

    context "when the key field is an integer" do
      before do
        {records, rsm_out} = RSMHelper.rsm_query(rsm_in(max: 20), shared.query,
                                                 :radius,
                                                 {:asc, :created_at})
        {:ok, records: records, rsm_out: rsm_out}
      end

      it do: rsm_out(shared.rsm_out, :first) |> should(be_binary())
      it do: rsm_out(shared.rsm_out, :last) |> should(be_binary())
    end
  end

  defp setup_query(shared, rsm_in) do
    {records, rsm_out} = RSMHelper.rsm_query(rsm_in, shared.query,
                                             :id, {:asc, :created_at})
    {:ok, records: records, rsm_out: rsm_out}
  end
end
