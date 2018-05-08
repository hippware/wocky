# credo:disable-for-this-file Credo.Check.Refactor.PipeChainStart
defmodule Wocky.BlockingSpec do
  use ESpec, async: true

  alias Wocky.Bot
  alias Wocky.Bot.Item
  alias Wocky.Bot.Share
  alias Wocky.Blocking
  alias Wocky.HomeStream
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Roster
  alias Wocky.User

  before do
    [alice, eve] = Factory.insert_list(2, :user)

    RosterHelper.make_friends(alice, eve)

    Enum.each([{alice, eve}, {eve, alice}], fn {a, b} ->
      bot = Factory.insert(:bot, %{user: a})
      Bot.subscribe(bot, b)

      Factory.insert(:item, %{bot: bot, user: b})
      Factory.insert(:share, %{bot: bot, sharer: a, user: b})
      Factory.insert(:home_stream_item, %{user: b, reference_bot: bot})
      Factory.insert(:home_stream_item, %{user: b, reference_user: a})

      Factory.insert(:home_stream_item, %{
        user: b,
        reference_user: a,
        reference_bot: bot
      })
    end)

    {:ok, alice: alice, eve: eve}
  end

  describe "block/2" do
    context "valid users" do
      before do
        result = Blocking.block(shared.alice, shared.eve)
        {:ok, result: result}
      end

      it "should return ok" do
        shared.result |> should(eq :ok)
      end

      it "should stop them being friends" do
        Roster.friends(shared.alice.id) |> should(eq [])
        Roster.friends(shared.eve.id) |> should(eq [])
      end

      it "should remove all HS references for the blocked user's bots and msgs" do
        HomeStream.get(shared.alice.id)
        |> Enum.each(&(&1.class |> should(eq :deleted)))

        HomeStream.get(shared.eve.id)
        |> Enum.each(&(&1.class |> should(eq :deleted)))
      end

      it "should delete all items on the user's bots by the blocked author" do
        shared.alice |> User.get_owned_bots() |> Item.get() |> should(eq [])
        shared.eve |> User.get_owned_bots() |> Item.get() |> should(eq [])
      end

      it "should delete all shares to blocked user's bots" do
        Repo.get_by(Share, user_id: shared.alice.id) |> should(be_nil())
        Repo.get_by(Share, user_id: shared.eve.id) |> should(be_nil())
      end

      it "should delete all subscriptions to blocked user's bots" do
        User.get_subscriptions(shared.alice) |> should(be_empty())
        User.get_subscriptions(shared.eve) |> should(be_empty())
      end

      it "should set the apprpriate blocking groups" do
        {a, e} = Roster.get_pair(shared.alice.id, shared.eve.id)
        a.groups |> should(eq [Roster.blocked_group()])
        e.groups |> should(eq [Roster.blocked_by_group()])
      end
    end

    context "invalid users" do
      it "should not crash" do
        Blocking.block(shared.alice, Factory.build(:user)) |> should(eq :ok)
        Blocking.block(Factory.build(:user), shared.alice) |> should(eq :ok)
      end
    end
  end

  describe "unblock/2" do
    it "should remove the blocking groups" do
      Blocking.unblock(shared.alice, shared.eve) |> should(eq :ok)
      {a, e} = Roster.get_pair(shared.alice.id, shared.eve.id)
      a.groups |> should(eq [])
      e.groups |> should(eq [])
    end

    it "should not crash with invalid users" do
      Blocking.unblock(shared.alice, Factory.build(:user)) |> should(eq :ok)
      Blocking.unblock(Factory.build(:user), shared.alice) |> should(eq :ok)
    end
  end
end
