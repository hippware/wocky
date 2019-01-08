defmodule Wocky.Push.EventsTest do
  use ExUnit.Case, async: true
  use Wocky.DataCase

  alias Wocky.Push.Event
  alias Wocky.Push.Events.BotInviteEvent
  alias Wocky.Push.Events.BotPerimeterEvent
  alias Wocky.Push.Events.NewMessageEvent
  alias Wocky.Push.Events.UserInvitationEvent
  alias Wocky.Repo.Factory

  @test_handle "test_handle"
  @test_title "test_title"
  @test_body "test body"
  @test_server "localhost"

  setup do
    u = Factory.build(:user, handle: @test_handle)
    b = Factory.build(:bot, title: @test_title)

    {:ok, user: u, bot: b}
  end

  describe "BotPerimeterEvent" do
    test "returns an appropriate message when entering", %{user: u, bot: b} do
      msg = Event.message(%BotPerimeterEvent{user: u, bot: b, event: :enter})
      assert msg =~ "is at"
      assert msg =~ @test_handle
      assert msg =~ @test_title
    end

    test "returns an appropriate message when exiting", %{user: u, bot: b} do
      msg = Event.message(%BotPerimeterEvent{user: u, bot: b, event: :exit})
      assert msg =~ "left"
      assert msg =~ @test_handle
      assert msg =~ @test_title
    end

    test "returns an appropriate URI", %{user: u, bot: b} do
      uri = Event.uri(%BotPerimeterEvent{user: u, bot: b, event: :exit})
      assert uri =~ "/bot"
      assert uri =~ "/#{@test_server}"
      assert uri =~ "/#{b.id}"
      assert uri =~ "/visitors"
    end
  end

  describe "NewMessageEvent" do
    setup do
      {:ok, sender: Factory.build(:user)}
    end

    test "uses 'Someone' when there is no user handle" do
      assert Event.message(%NewMessageEvent{}) =~ "Someone"
      assert Event.message(%NewMessageEvent{content: "foo"}) =~ "From: Someone"
    end

    test "assumes sending an image when there is no body" do
      assert Event.message(%NewMessageEvent{}) =~ "sent you an image!"
    end

    test "returns an appropriate message", %{user: u} do
      msg = Event.message(%NewMessageEvent{from: u, content: @test_body})
      assert msg =~ @test_handle
      assert msg =~ @test_body
    end

    test "returns an appropriate URI", %{user: u, sender: sender} do
      uri = Event.uri(%NewMessageEvent{to: u, from: sender})
      assert uri =~ "/conversation"
      assert uri =~ "/#{@test_server}"
      assert uri =~ "/#{sender.id}"
    end
  end

  describe "UserInvitationEvent" do
    setup %{user: inviter} do
      {:ok, user: Factory.build(:user), inviter: inviter}
    end

    test "returns an appropriate message", %{user: u, inviter: i} do
      msg = Event.message(%UserInvitationEvent{user: u, from: i})
      assert msg =~ @test_handle
      assert msg =~ "invited you to be their friend"
    end

    test "returns an appropriate uri", %{user: u, inviter: i} do
      uri = Event.uri(%UserInvitationEvent{user: u, from: i})
      assert uri =~ "/invitations"
    end
  end

  describe "BotInviteEvent" do
    test "uses 'Someone' when there is no user handle" do
      assert Event.message(%BotInviteEvent{}) =~ "Someone"
    end

    test "returns an appropriate message", %{user: u} do
      msg = Event.message(%BotInviteEvent{from: u})
      assert msg =~ "invited you to follow"
      assert msg =~ @test_handle
    end

    test "returns an appropriate URI", %{user: u, bot: b} do
      uri = Event.uri(%BotInviteEvent{from: u, bot: b})
      assert uri =~ "/bot"
      assert uri =~ "/#{@test_server}"
      assert uri =~ "/#{b.id}"
    end
  end
end
