defmodule Wocky.Push.BackendTest do
  use ExUnit.Case, async: true

  alias Faker.Lorem
  alias Pigeon.APNS.Notification, as: APNSNotification
  alias Pigeon.FCM.Notification, as: FCMNotification
  alias Wocky.Push.Event
  alias Wocky.Push.Events.NewMessageEvent
  alias Wocky.Push.Backend.{APNS, FCM}
  alias Wocky.Repo.{Factory, ID}

  describe "build_notification/2" do
    setup do
      u = Factory.build(:user)
      content = Lorem.paragraph()
      event = %NewMessageEvent{from: u, content: content}

      token = ID.new()

      {:ok, event: event, token: token}
    end

    test "FCM should create a notification with the correct parameters", ctx do
      n = FCM.build_notification(ctx.event, ctx.token)
      url = Event.uri(ctx.event)
      body = Event.message(ctx.event)

      assert %FCMNotification{
               payload: %{
                 "data" => %{"url" => ^url},
                 "notification" => %{
                   "body" => ^body,
                   "title" => "tinyrobot"
                 }
               },
               restricted_package_name: "app"
             } = n
    end

    test "APNS should create a notification with the correct parameters", ctx do
      n = APNS.build_notification(ctx.event, ctx.token)
      url = Event.uri(ctx.event)
      body = Event.message(ctx.event)
      token = ctx.token

      assert %APNSNotification{
               device_token: ^token,
               payload: %{
                 "aps" => %{
                   "alert" => ^body,
                   "badge" => 1
                 },
                 "uri" => ^url
               },
               topic: "app"
             } = n
    end
  end
end
