defmodule Wocky.Notifier.Push.Backend.FCMTest do
  use ExUnit.Case, async: true

  alias Wocky.Notifier.Push.Backend.FCM
  alias Wocky.Repo.Factory

  @silent_events [
    :bot_invitation_event,
    :bot_invitation_response_event,
    :bot_item_event,
    :location_share_event,
    :user_invitation_event,
    :user_invitation_response_event
  ]

  describe "new message notifications" do
    setup do
      {:ok, data: make_payload_data(:new_message_event)}
    end

    test "have default sound", %{data: data} do
      assert Map.get(data, "sound") == "default"
    end

    test "have channel id of chat", %{data: data} do
      assert Map.get(data, "android_channel_id") == "chat"
    end
  end

  describe "geofence notifications" do
    setup do
      {:ok, data: make_payload_data(:geofence_event)}
    end

    test "have silent sound", %{data: data} do
      assert Map.get(data, "sound") == "silence"
    end

    test "have channel id of geofence", %{data: data} do
      assert Map.get(data, "android_channel_id") == "geofence"
    end
  end

  describe "all other notifications" do
    setup do
      datas = Enum.map(@silent_events, &make_payload_data(&1))

      {:ok, datas: datas}
    end

    test "have silent sound", %{datas: datas} do
      for data <- datas do
        assert Map.get(data, "sound") == "silence"
      end
    end

    test "have no channel id", %{datas: datas} do
      for data <- datas do
        refute Map.get(data, "android_channel_id")
      end
    end
  end

  defp make_payload_data(event_name) do
    event = Factory.build(event_name)
    notification = FCM.build_notification(event, "token")
    Map.get(notification.payload, "notification", %{})
  end
end
