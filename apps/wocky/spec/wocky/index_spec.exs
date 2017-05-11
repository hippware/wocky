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
    it do: Index.geosearch(1.0, 1.0) |> should(be_ok_result())
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

  describe "user_updated/2" do
    before do
      Index.user_updated(id(), Factory.build(:user))
    end

    it do: assert [{_, :users, :update, _}] = TestIndexer.get_index_operations
  end

  describe "user_removed/1" do
    before do
      Index.user_removed(id())
    end

    it do: assert [{_, :users, :delete, nil}] = TestIndexer.get_index_operations
  end

  describe "bot_updated/2" do
    before do
      Index.bot_updated(id(), Factory.build(:bot))
    end

    it do: assert [{_, :bots, :update, _}] = TestIndexer.get_index_operations
  end

  describe "bot_removed/1" do
    before do
      Index.bot_removed(id())
    end

    it do: assert [{_, :bots, :delete, nil}] = TestIndexer.get_index_operations
  end

  describe "unknown calls" do
    it do: GenServer.call(:wocky_index, :bogus) |> should(be_error_result())
    it do: GenServer.cast(:wocky_index, :bogus) |> should(eq :ok)
  end
end
