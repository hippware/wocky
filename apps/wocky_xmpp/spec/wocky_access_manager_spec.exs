# credo:disable-for-this-file Credo.Check.Refactor.PipeChainStart
defmodule :wocky_access_manager_spec do
  use ESpec, async: true
  use Wocky.JID

  alias Wocky.Bot
  alias Wocky.Bot.Invitation
  alias Wocky.Bot.Subscription
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.User

  before_all do
    :wocky_access_manager.register("loop", __MODULE__)
    :wocky_access_manager.register("overflow", __MODULE__)
  end

  before do
    alice = Factory.insert(:user)
    bob = Factory.insert(:user)
    bot = Factory.insert(:bot, user: alice)

    Invitation.put(bob, bot, alice)
    Subscription.put(bob, bot)

    {:ok, alice: alice, bob: bob, bot: bot}
  end

  finally do
    User.delete(shared.alice.id)
    User.delete(shared.bob.id)
  end

  def check_access("loop/1", _, _) do
    {:redirect, JID.make("", "localhost", "loop/2")}
  end

  def check_access("loop/2", _, _) do
    {:redirect, JID.make("", "localhost", "loop/1")}
  end

  def check_access("overflow/" <> i, _, _) do
    j = i |> String.to_integer() |> Kernel.+(1) |> Integer.to_string()
    {:redirect, JID.make("", "localhost", "overflow/" <> j)}
  end

  def run(bot_jid, user_jid, op) do
    :wocky_access_manager.check_access(bot_jid, user_jid, op)
  end

  describe "check_access/3" do
    let :bot_jid, do: Bot.to_jid(shared.bot)
    let :alice_jid, do: User.to_jid(shared.alice)

    it do: run(bot_jid(), alice_jid(), :view) |> should(eq :allow)
    it do: run(bot_jid(), alice_jid(), :delete) |> should(eq :allow)
    it do: run(bot_jid(), alice_jid(), :modify) |> should(eq :allow)

    let :bob_jid, do: User.to_jid(shared.bob)

    it do: run(bot_jid(), bob_jid(), :view) |> should(eq :allow)
    it do: run(bot_jid(), bob_jid(), :delete) |> should(eq :deny)
    it do: run(bot_jid(), bob_jid(), :modify) |> should(eq :deny)

    let :carol_jid, do: JID.make(ID.new(), "localhost")

    it do: run(bot_jid(), carol_jid(), :view) |> should(eq :deny)
    it do: run(bot_jid(), carol_jid(), :delete) |> should(eq :deny)
    it do: run(bot_jid(), carol_jid(), :modify) |> should(eq :deny)

    context "with a redirect loop" do
      let :user, do: JID.make("", "localhost", "loop/1")
      it do: run(user(), alice_jid(), :view) |> should(eq :deny)
    end

    context "with a redirect overflow", slow: true do
      let :user, do: JID.make("", "localhost", "overflow/1")
      it do: run(user(), alice_jid(), :view) |> should(eq :deny)
    end
  end
end
