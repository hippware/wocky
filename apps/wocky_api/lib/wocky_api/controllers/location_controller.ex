defmodule WockyAPI.Controllers.LocationController do
  use WockyAPI, :controller

  alias Wocky.Location
  alias Wocky.Location.UserLocation

  action_fallback WockyAPI.Controllers.FallbackController

  def create(conn, params) do
    user_id = conn.assigns.current_user_id
    device = params["device"]
    locations = normalize_location(params["location"])

    if device do
      _ = process_locations(user_id, device, locations)

      send_resp(conn, :created, "")
    else
      {:error, :missing_keys}
    end
  end

  defp normalize_location(locations) when is_list(locations), do: locations
  defp normalize_location(location), do: [location]

  defp process_locations(user_id, device, locations) do
    length = length(locations)

    for {l, idx} <- Enum.with_index(locations, 1) do
      if has_required_keys(l) do
        loc = make_location(l, device)

        {:ok, _} = Location.set_user_location(user_id, loc, idx == length)
      end
    end
  end

  defp has_required_keys(%{"coords" => coords}) do
    Map.get(coords, "latitude") && Map.get(coords, "longitude") &&
      Map.get(coords, "accuracy")
  end

  defp has_required_keys(_), do: false

  defp make_location(%{"coords" => c} = l, device) do
    %{
      lat: c["latitude"],
      lon: c["longitude"],
      accuracy: c["accuracy"],
      speed: c["speed"],
      heading: c["heading"],
      altitude: c["altitude"],
      altitude_accuracy: c["altitude_accuracy"],
      captured_at: l["timestamp"],
      uuid: l["uuid"],
      odometer: l["odometer"],
      is_moving: l["is_moving"],
      device: device
    }
    |> UserLocation.new()
    |> maybe_add_activity(l["activity"])
    |> maybe_add_battery(l["battery"])
    |> add_extra_fields(l)
  end

  defp maybe_add_activity(l, %{} = a) do
    %UserLocation{l | activity: a["type"], activity_confidence: a["confidence"]}
  end

  defp maybe_add_activity(l, _), do: l

  defp maybe_add_battery(l, %{} = b) do
    %UserLocation{
      l
      | battery_level: b["level"],
        battery_charging: b["is_charging"]
    }
  end

  defp maybe_add_battery(l, _), do: l

  defp add_extra_fields(l, input) do
    extra_fields =
      Map.drop(input, [
        "coords",
        "timestamp",
        "uuid",
        "odometer",
        "is_moving",
        "activity",
        "battery"
      ])

    %UserLocation{l | extra_fields: extra_fields}
  end
end
