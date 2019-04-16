defmodule Wocky.Notifier.Push.EventsTest do
  use ExUnit.Case, async: true
  use Wocky.DataCase

  alias Wocky.Events.{
    BotInvitation,
    GeofenceEvent,
    NewMessage,
    UserInvitation
  }

  alias Wocky.Notifier.Push.Event
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

  describe "GeofenceEvent" do
    test "returns an appropriate message when entering", %{user: u, bot: b} do
      msg = Event.message(%GeofenceEvent{from: u, bot: b, event: :enter})
      assert msg =~ "is at"
      assert msg =~ @test_handle
      assert msg =~ @test_title
    end

    test "returns an appropriate message when exiting", %{user: u, bot: b} do
      msg = Event.message(%GeofenceEvent{from: u, bot: b, event: :exit})
      assert msg =~ "left"
      assert msg =~ @test_handle
      assert msg =~ @test_title
    end

    test "returns an appropriate URI", %{user: u, bot: b} do
      uri = Event.uri(%GeofenceEvent{from: u, bot: b, event: :exit})
      assert uri =~ "/bot"
      assert uri =~ "/#{@test_server}"
      assert uri =~ "/#{b.id}"
      assert uri =~ "/visitors"
    end
  end

  describe "NewMessage" do
    setup do
      {:ok, sender: Factory.build(:user)}
    end

    test "uses 'Someone' when there is no user handle" do
      assert Event.message(%NewMessage{}) =~ "Someone"
      assert Event.message(%NewMessage{content: "foo"}) =~ "From: Someone"
    end

    test "assumes sending an image when there is no body" do
      assert Event.message(%NewMessage{}) =~ "sent you an image!"
    end

    test "returns an appropriate message", %{user: u} do
      msg = Event.message(%NewMessage{from: u, content: @test_body})
      assert msg =~ @test_handle
      assert msg =~ @test_body
    end

    test "returns an appropriate URI", %{user: u, sender: sender} do
      uri = Event.uri(%NewMessage{to: u, from: sender})
      assert uri =~ "/conversation"
      assert uri =~ "/#{@test_server}"
      assert uri =~ "/#{sender.id}"
    end
  end

  describe "UserInvitation" do
    setup %{user: inviter} do
      {:ok, to: Factory.build(:user), inviter: inviter}
    end

    test "returns an appropriate message", %{to: u, inviter: i} do
      msg = Event.message(%UserInvitation{to: u, from: i})
      assert msg =~ @test_handle
      assert msg =~ "invited you to be their friend"
    end

    test "returns an appropriate uri", %{to: u, inviter: i} do
      uri = Event.uri(%UserInvitation{to: u, from: i})
      assert uri =~ "/invitations"
    end
  end

  describe "BotInvitation" do
    test "uses 'Someone' when there is no user handle" do
      assert Event.message(%BotInvitation{}) =~ "Someone"
    end

    test "returns an appropriate message", %{user: u} do
      msg = Event.message(%BotInvitation{from: u})
      assert msg =~ "invited you to follow"
      assert msg =~ @test_handle
    end

    test "returns an appropriate URI", %{user: u, bot: b} do
      uri = Event.uri(%BotInvitation{from: u, bot: b})
      assert uri =~ "/bot"
      assert uri =~ "/#{@test_server}"
      assert uri =~ "/#{b.id}"
    end
  end
end
