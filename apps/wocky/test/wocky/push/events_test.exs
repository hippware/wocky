defmodule Wocky.Push.EventsTest do
  use ExUnit.Case, async: true
  use Wocky.DataCase

  alias Wocky.Repo.Factory
  alias Wocky.Push.Event
  alias Wocky.Push.Events.BotPerimeterEvent
  alias Wocky.Push.Events.BotShareEvent
  alias Wocky.Push.Events.NewMessageEvent

  @test_handle "test_handle"
  @test_title "test_title"
  @test_body "test body"

  setup do
    u = Factory.build(:user, handle: @test_handle)
    b = Factory.build(:bot, title: @test_title)

    {:ok, user: u, bot: b}
  end

  describe "BotPerimeterEvent" do
    test "returns an appropriate message when entering", %{user: u, bot: b} do
      msg = Event.message(%BotPerimeterEvent{user: u, bot: b, event: :enter})
      assert msg =~ "is near"
      assert msg =~ @test_handle
      assert msg =~ @test_title
    end

    test "returns an appropriate message when exiting", %{user: u, bot: b} do
      msg = Event.message(%BotPerimeterEvent{user: u, bot: b, event: :exit})
      assert msg =~ "is leaving"
      assert msg =~ @test_handle
      assert msg =~ @test_title
    end

    test "returns an appropriate URI", %{user: u, bot: b} do
      uri = Event.uri(%BotPerimeterEvent{user: u, bot: b, event: :exit})
      assert uri =~ "/bot/"
      assert uri =~ b.id
    end
  end

  describe "BotShareEvent" do
    test "uses 'Someone' when there is no user handle" do
      assert Event.message(%BotShareEvent{}) =~ "Someone"
    end

    test "returns an appropriate message", %{user: u} do
      msg = Event.message(%BotShareEvent{from: u})
      assert msg =~ "shared a bot with you!"
      assert msg =~ @test_handle
    end

    test "returns an appropriate URI", %{user: u, bot: b} do
      uri = Event.uri(%BotShareEvent{from: u, bot: b})
      assert uri =~ "/bot/"
      assert uri =~ b.id
    end
  end

  describe "NewMessageEvent" do
    setup %{user: u} do
      {:ok, _} = Repo.insert(u)

      oj = Factory.new_jid
      c = Factory.insert(:conversation, user_id: u.id, other_jid: oj)

      {:ok, cid: c.id, other_jid: oj}
    end

    test "uses 'Someone' when there is no user handle" do
      assert Event.message(%NewMessageEvent{}) =~ "Someone"
      assert Event.message(%NewMessageEvent{body: "foo"}) =~ "From: Someone"
    end

    test "assumes sending an image when there is no body" do
      assert Event.message(%NewMessageEvent{}) =~ "sent you an image!"
    end

    test "returns an appropriate message", %{user: u} do
      msg = Event.message(%NewMessageEvent{from: u, body: @test_body})
      assert msg =~ @test_handle
      assert msg =~ @test_body
    end

    test "returns an appropriate URI", %{user: u, other_jid: oj, cid: cid} do
      uri = Event.uri(%NewMessageEvent{to: u, from: oj, conversation_id: cid})
      assert uri =~ "/conversation/"
      assert uri =~ "/#{Integer.to_string(cid)}"
    end
  end
end
