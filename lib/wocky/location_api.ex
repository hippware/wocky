defmodule Wocky.LocationApi do

  alias Wocky.User
  alias Wocky.Location

  defmodule State do
    defstruct [user: nil, resource: nil, coords: nil]
  end

  def start do
    dispatch = :cowboy_router.compile([
        {:_, [
            {"/api/v1/users/[:user_id]/location", Wocky.LocationApi, []}
        ]}
    ])

    {:ok, _} = :cowboy.start_http(:location_api, 100, [port: 8080],
                                  [env: [dispatch: dispatch]])
    :ok
  end

  def init(_transport, _req, []) do
    {:upgrade, :protocol, :cowboy_rest}
  end

  def rest_init(req, _opts) do
    {:ok, req, %State{}}
  end

  def allowed_methods(req, state) do
    {["POST"], req, state}
  end

  def allow_missing_post(req, state) do
    {true, req, state}
  end

  def content_types_accepted(req, state) do
    {[{"application/json", :from_json}], req, state}
  end

  def resource_exists(req, state) do
    {user_id, req} = :cowboy_req.binding(:user_id, req, nil)
    case User.get(user_id) do
      nil -> {false, req, state}
      user -> {true, req, %State{state | user: user}}
    end
  end

  def malformed_request(req, state) do
    {:ok, body, _req2} = :cowboy_req.body(req)
    case Poison.Parser.parse(body, keys: :atoms) do
      {:ok, data} ->
        location = extract_location_object(data[:location])
        if has_required_keys?(location, data[:resource]) do
          {false, req, %State{state |
            resource: data.resource,
            coords: location.coords}}
        else
          {true, req, state}
        end

      {:error, _} ->
        {true, req, state}
    end
  end

  defp extract_location_object([location]),               do: location
  defp extract_location_object(list) when is_list(list),  do: nil
  defp extract_location_object(nil),                      do: nil
  defp extract_location_object(location),                 do: location

  defp has_required_keys?(%{coords: coords}, resource) do
    Map.get(coords, :latitude) &&
    Map.get(coords, :longitude) &&
    Map.get(coords, :accuracy) &&
    resource
  end
  defp has_required_keys?(_, _), do: false

  def is_authorized(req, state) do
    {auth_user, _} = :cowboy_req.header("x-auth-user", req, nil)
    {auth_token, _} = :cowboy_req.header("x-auth-token", req, nil)
    {user_id, _} = :cowboy_req.binding(:user_id, req, nil)

    if check_token(auth_user, auth_token) && auth_user === user_id do
      {true, req, state}
    else
      {{false, "API Token"}, req, state}
    end
  end

  defp check_token(nil, _), do: false
  defp check_token(_, nil), do: false
  defp check_token(user, token),
    do: :wocky_db_user.check_token(user, :wocky_app.server, token)

  def from_json(req, %State{user: user, coords: coords} = state) do
    location = %Location{
      lat: coords.latitude,
      lon: coords.longitude,
      accuracy: coords.accuracy
    }
    user = %User{user | resource: state.resource}
    :ok = Location.user_location_changed(user, location)
    {true, req, state}
  end
end
