defmodule Wocky.Notifier.Push.BackendTest do
  use ExUnit.Case, async: true

  alias Faker.Lorem
  alias Pigeon.APNS.Notification, as: APNSNotification
  alias Pigeon.FCM.Notification, as: FCMNotification
  alias Wocky.Events.LocationRequest
  alias Wocky.Events.NewMessage
  alias Wocky.Notifier.Push.Backend.APNS
  alias Wocky.Notifier.Push.Backend.FCM
  alias Wocky.Notifier.Push.Event
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID

  setup do
    user = Factory.build(:user)
    token = ID.new()
    {:ok, user: user, token: token}
  end

  describe "build_notification/2 for standard message" do
    setup ctx do
      content = Lorem.paragraph()
      event = %NewMessage{from: ctx.user, content: content}

      {:ok, event: event}
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

  describe "build_notification/2 for background message" do
    setup ctx do
      event = %LocationRequest{to: ctx.user}

      {:ok, event: event}
    end

    test "FCM should create a notification with the correct parameters", ctx do
      n = FCM.build_notification(ctx.event, ctx.token)

      assert %FCMNotification{
               payload: %{
                 "data" => %{"location-request" => 1}
               },
               restricted_package_name: "app"
             } = n

      refute Map.has_key?(n.payload, "notification")
    end

    test "APNS should create a notification with the correct parameters", ctx do
      n = APNS.build_notification(ctx.event, ctx.token)
      token = ctx.token

      assert %APNSNotification{
               device_token: ^token,
               payload: %{
                 "aps" => %{
                   "content-available" => 1
                 },
                 "location-request" => 1
               },
               topic: "app"
             } = n

      refute Map.has_key?(n.payload["aps"], "badge")
    end
  end
end
