defmodule Wocky.IndexSpec do
  use ESpec, async: false

  alias Ecto.Adapters.SQL.Sandbox
  alias Wocky.Index
  alias Wocky.Index.TestIndexer
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID

  let :indexer, do: GenServer.whereis(:wocky_index)
  let! :id, do: ID.new

  before do
    TestIndexer.reset
  end

  describe "geosearch/2" do
    before do
      Sandbox.allow(Repo, self(), indexer())

      bots = Factory.insert_list(100, :bot)
      bot = hd(bots)
      result = Index.geosearch(bot.lat, bot.lon)

      {:ok, bot: bot, result: result}
    end

    it "should return a success result" do
      shared.result |> should(be_ok_result())
    end

    it "should find a bot that contains the point" do
      {:ok, bots} = shared.result

      bots |> should(have_size 1)
      bots |> hd |> Map.get("objectID") |> should(eq shared.bot.id)
    end
  end

  describe "reindex/1" do
    before do
      Sandbox.allow(Repo, self(), indexer())
      user = Factory.insert(:user)
      _bot = Factory.insert(:bot, user: user)
    end

    it do: :bogus |> Index.reindex |> should(be_error_result())
    it do: :users |> Index.reindex |> should(eq :ok)
    it do: :bots |> Index.reindex |> should(eq :ok)
  end

  describe "update/3" do
    context "users" do
      before do
        Index.update(:user, id(), Factory.build(:user))
      end

      it do: assert [{_, :users, :update, _}] = TestIndexer.get_index_operations
    end

    context "bots" do
      before do
        Index.update(:bot, id(), Factory.build(:bot))
      end

      it do: assert [{_, :bots, :update, _}] = TestIndexer.get_index_operations
    end
  end

  describe "remove/2" do
    context "users" do
      before do
        Index.remove(:user, id())
      end

      it do: assert [{_, :users, :delete, nil}] = TestIndexer.get_index_operations
    end

    context "bots" do
      before do
        Index.remove(:bot, id())
      end

      it do: assert [{_, :bots, :delete, nil}] = TestIndexer.get_index_operations
    end
  end

  describe "unknown calls" do
    it do: GenServer.call(:wocky_index, :bogus) |> should(be_error_result())
    it do: GenServer.cast(:wocky_index, :bogus) |> should(eq :ok)
  end
end
