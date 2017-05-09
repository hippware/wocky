defmodule :access_query_spec do
  use ESpec, async: true
  use Wocky.JID

  import :access_query, only: [run: 3]

  alias Wocky.Bot
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.User

  @alice_id ID.new
  @bob_id ID.new
  @bot_id ID.new

  @alice_jid JID.make(@alice_id, "localhost")
  @bob_jid JID.make(@bob_id, "localhost")
  @carol_jid JID.make(ID.new, "localhost")
  @bot_jid JID.make("", "localhost", "bot/" <> @bot_id)

  before_all do
    alice = Factory.insert(:user, %{id: @alice_id, username: @alice_id})
    bob = Factory.insert(:user, %{id: @bob_id, username: @bob_id})
    bot = Factory.insert(:bot, %{id: @bot_id, user: alice})

    Bot.share(bot, bob, alice)
    User.subscribe(bob, bot)

    :mod_wocky_access.register("loop", __MODULE__)
    :mod_wocky_access.register("overflow", __MODULE__)
    :mod_wocky_access.register("timeout", __MODULE__)
  end

  after_all do
    User.delete(@alice_id)
    User.delete(@bob_id)
  end

  def check_access("loop/1", _, _) do
    {:redirect, JID.make("", "localhost", "loop/2")}
  end
  def check_access("loop/2", _, _) do
    {:redirect, JID.make("", "localhost", "loop/1")}
  end
  def check_access("overflow" <> i, _, _) do
    j = i |> String.to_integer |> Kernel.+(1) |> Integer.to_string
    {:redirect, JID.make("", "localhost", "overflow/" <> j)}
  end
  def check_access("timeout", _, _) do
    Process.sleep(2000)
  end

  describe "run/3" do
    it do: run(@bot_jid, @alice_jid, :view) |> should(eq :allow)
    it do: run(@bot_jid, @alice_jid, :delete) |> should(eq :allow)
    it do: run(@bot_jid, @alice_jid, :modify) |> should(eq :allow)

    it do: run(@bot_jid, @bob_jid, :view) |> should(eq :allow)
    it do: run(@bot_jid, @bob_jid, :delete) |> should(eq :deny)
    it do: run(@bot_jid, @bob_jid, :modify) |> should(eq :deny)

    it do: run(@bot_jid, @carol_jid, :view) |> should(eq :deny)
    it do: run(@bot_jid, @carol_jid, :delete) |> should(eq :deny)
    it do: run(@bot_jid, @carol_jid, :modify) |> should(eq :deny)

    context "with a redirect loop" do
      let :user, do: JID.make("", "localhost", "loop/1")
      it do: run(user(), @alice_jid, :view) |> should(eq :deny)
    end

    context "with a redirect overflow", slow: true do
      let :user, do: JID.make("", "localhost", "overflow/1")
      it do: run(user(), @alice_jid, :view) |> should(eq :deny)
    end

    context "with a timeout", slow: true do
      let :user, do: JID.make("", "localhost", "timeout")
      it do: run(user(), @alice_jid, :view) |> should(eq :deny)
    end
  end
end
