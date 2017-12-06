defmodule Wocky.Push.EventsTest do
  use ExUnit.Case, async: true

  alias Wocky.Bot
  alias Wocky.Push.Event
  alias Wocky.Push.Events.BotPerimeterEvent
  alias Wocky.Push.Events.BotShareEvent
  alias Wocky.Push.Events.NewMessageEvent
  alias Wocky.Repo.ID
  alias Wocky.User

  @test_handle "test_handle"
  @test_title "test_title"
  @test_body "test body"

  setup do
    u = %User{handle: @test_handle}
    b = %Bot{id: ID.new, title: @test_title}

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

    test "returns an appropriate URI", %{user: u} do
      uri = Event.uri(%NewMessageEvent{from: u})
      assert uri =~ "/conversation/"
      assert uri =~ "NONE"
    end
  end
end
