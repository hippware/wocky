defmodule WockyAPI.LocationController do
  use WockyAPI, :controller

  alias Wocky.User
  alias Wocky.User.Location

  action_fallback WockyAPI.FallbackController

  def create(conn, params) do
    user = conn.assigns.current_user
    device = params["device"]
    locations = normalize_location(params["location"])

    if device do
      _ = process_locations(user, device, locations)

      send_resp(conn, :created, "")
    else
      {:error, :missing_keys}
    end
  end

  defp normalize_location(locations) when is_list(locations), do: locations
  defp normalize_location(location), do: [location]

  defp process_locations(user, device, locations) do
    length = length(locations)

    for {l, idx} <- Enum.with_index(locations, 1) do
      if has_required_keys(l) do
        loc = make_location(l, device)

        {:ok, _} = User.set_location(user, loc, idx == length)
      end
    end
  end

  defp has_required_keys(%{"coords" => coords}) do
    Map.get(coords, "latitude") && Map.get(coords, "longitude") &&
      Map.get(coords, "accuracy")
  end

  defp has_required_keys(_), do: false

  defp make_location(%{"coords" => c} = l, device) do
    %Location{
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
    |> maybe_add_activity(l["activity"])
    |> maybe_add_battery(l["battery"])
  end

  defp maybe_add_activity(l, %{} = a) do
    %Location{l | activity: a["type"], activity_confidence: a["confidence"]}
  end

  defp maybe_add_activity(l, _), do: l

  defp maybe_add_battery(l, %{} = b) do
    %Location{l | battery_level: b["level"], battery_charging: b["is_charging"]}
  end

  defp maybe_add_battery(l, _), do: l
end
