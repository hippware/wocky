defmodule WockyAPI.LocationController do
  use WockyAPI, :controller

  alias Wocky.Repo
  alias Wocky.User

  action_fallback WockyAPI.FallbackController

  def create(conn, params) do
    with {:ok, c, rsrc} <- extract_values(params) do
      # This can't return nil since the token check passed.
      user = Repo.get(User, conn.assigns.current_user)
      :ok = User.set_location(user, rsrc, c["latitude"],
                              c["longitude"], c["accuracy"])

      send_resp(conn, :created, "")
    end
  end

  defp extract_values(params) do
    params["location"]
    |> do_extract_values(params["resource"])
    |> handle_parse_result()
  end

  defp do_extract_values([location | _], resource), do: {location, resource}
  defp do_extract_values(location, resource),       do: {location, resource}

  defp handle_parse_result({location, resource}) do
    if has_required_keys(location, resource) do
      {:ok, location["coords"], resource}
    else
      {:error, :missing_keys}
    end
  end

  defp has_required_keys(%{"coords" => coords}, resource) do
    Map.get(coords, "latitude") &&
    Map.get(coords, "longitude") &&
    Map.get(coords, "accuracy") &&
    resource
  end
  defp has_required_keys(_, _), do: false
end
