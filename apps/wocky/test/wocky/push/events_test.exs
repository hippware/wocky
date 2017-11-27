defmodule Wocky.Push.EventsTest do
  use ExUnit.Case, async: true

  alias Wocky.Bot
  alias Wocky.Push.Event
  alias Wocky.Push.Events.BotPerimeterEvent
  alias Wocky.Push.Events.BotShareEvent
  alias Wocky.Push.Events.NewMessageEvent
  alias Wocky.User

  @test_handle "test_handle"
  @test_title "test_title"
  @test_body "test body"

  setup do
    u = %User{handle: @test_handle}
    b = %Bot{title: @test_title}

    {:ok, user: u, bot: b}
  end

  describe "BotPerimeterEvent when entering" do
    setup %{user: u, bot: b} do
      msg = Event.format(%BotPerimeterEvent{user: u, bot: b, event: :enter})
      {:ok, msg: msg}
    end

    test "contains an appropriate message", %{msg: msg} do
      assert msg =~ "is near"
      assert msg =~ @test_handle
      assert msg =~ @test_title
    end
  end

  describe "BotPerimeterEvent when exiting" do
    setup %{user: u, bot: b} do
      msg = Event.format(%BotPerimeterEvent{user: u, bot: b, event: :exit})
      {:ok, msg: msg}
    end

    test "contains an appropriate message", %{msg: msg} do
      assert msg =~ "is leaving"
      assert msg =~ @test_handle
      assert msg =~ @test_title
    end
  end

  describe "BotShareEvent" do
    setup %{user: u} do
      msg = Event.format(%BotShareEvent{from: u})
      {:ok, msg: msg}
    end

    test "uses 'Someone' when there is no user handle" do
      assert Event.format(%BotShareEvent{}) =~ "Someone"
    end

    test "contains an appropriate message", %{msg: msg} do
      assert msg =~ "shared a bot with you!"
      assert msg =~ @test_handle
    end
  end

  describe "NewMessageEvent" do
    setup %{user: u} do
      msg = Event.format(%NewMessageEvent{from: u, body: @test_body})
      {:ok, msg: msg}
    end

    test "uses 'Someone' when there is no user handle" do
      assert Event.format(%NewMessageEvent{}) =~ "Someone"
      assert Event.format(%NewMessageEvent{body: "foo"}) =~ "From: Someone"
    end

    test "assumes sending an image when there is no body" do
      assert Event.format(%NewMessageEvent{}) =~ "sent you an image!"
    end

    test "contains an appropriate message", %{msg: msg} do
      assert msg =~ @test_handle
      assert msg =~ @test_body
    end
  end
end
