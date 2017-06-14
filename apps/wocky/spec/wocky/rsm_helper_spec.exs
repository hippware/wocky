defmodule Wocky.RSMHelperSpec do
  use ESpec, async: false
  use Wocky.Repo.Model

  import Wocky.RSMHelper, only: [rsm_query: 4]

  require Record

  alias Wocky.Bot
  alias Wocky.Repo.Factory

  @rsm_hdr "ejabberd/include/jlib.hrl"
  @count 20

  Record.defrecord :rsm_in,  Record.extract(:rsm_in, from_lib: @rsm_hdr)
  Record.defrecord :rsm_out, Record.extract(:rsm_out, from_lib: @rsm_hdr)

  before do
    user = Factory.insert(:user)
    Factory.insert_list(@count, :bot, user: user)

    bots =
      Bot
      |> where(user_id: ^user.id)
      |> order_by(asc: :created_at)
      |> Repo.all

    {:ok,
      user: user,
      bots: bots}
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
  end

  defp setup_query(shared, rsm_in) do
    query = where(Bot, [user_id: ^shared.user.id])
    {records, rsm_out} = rsm_query(rsm_in, query, :id, {:asc, :created_at})
    {:ok, records: records, rsm_out: rsm_out}
  end
end
