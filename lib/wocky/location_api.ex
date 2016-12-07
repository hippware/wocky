defmodule Wocky.LocationApi do
  @moduledoc "HTTP API implementation for sending user location updates"

  use Exref, ignore: [
    init: 3, rest_init: 2, allow_missing_post: 2, allowed_methods: 2,
    content_types_accepted: 2, from_json: 2, is_authorized: 2,
    malformed_request: 2, resource_exists: 2
  ]
  import OK, only: :macros
  alias Wocky.User
  alias Wocky.Location

  defmodule State do
    @moduledoc false
    defstruct [user: nil, resource: nil, coords: nil]
  end

  def start do
    dispatch = :cowboy_router.compile([
        {:_, [
            {"/api/v1/users/[:user_id]/location", Wocky.LocationApi, []}
        ]}
    ])

    port = Application.get_env(:wocky, :location_api_port)
    {:ok, _} = :cowboy.start_http(:location_api, 100, [port: port],
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

  defp set_response_text(req, error_text) do
    req = :cowboy_req.set_resp_header("content-type", "text/plain", req)
    :cowboy_req.set_resp_body(error_text, req)
  end

  def resource_exists(req, state) do
    {user_id, req} = :cowboy_req.binding(:user_id, req, nil)
    case User.get(user_id) do
      nil ->
        {false, set_response_text(req, "User #{user_id} not found."), state}

      user ->
        {true, req, %State{state | user: user}}
    end
  end

  def malformed_request(req, state) do
    case req |> read_and_parse_body do
      {:ok, coords, resource} ->
        {false, req, %State{state | resource: resource, coords: coords}}

      {:error, :timeout} ->
        {true, set_response_text(req, "Timeout reading request body."), state}

      {:error, :missing_keys} ->
        {true, set_response_text(req, "JSON missing required keys."), state}

      {:error, _} ->
        {true, set_response_text(req, "JSON payload can not be parsed."), state}
    end
  end

  defp read_and_parse_body(request) do
    request
    |> read_body
    ~>> parse_body
    ~>> extract_values
    ~>> handle_parse_result
  end

  defp read_body(req) do
    case :cowboy_req.body(req) do
      {:ok, body, _} -> {:ok, body}
      {:error, _} = error -> error
    end
  end

  defp parse_body(body), do: body |> Poison.Parser.parse(keys: :atoms)

  defp extract_values(data),
    do: {:ok, extract_values(data[:location], data[:resource])}

  defp extract_values([location | _], resource), do: {location, resource}
  defp extract_values(location, resource),       do: {location, resource}

  defp handle_parse_result({location, resource}) do
    if has_required_keys(location, resource) do
      {:ok, location.coords, resource}
    else
      {:error, :missing_keys}
    end
  end

  defp has_required_keys(%{coords: coords}, resource) do
    Map.get(coords, :latitude) &&
    Map.get(coords, :longitude) &&
    Map.get(coords, :accuracy) &&
    resource
  end
  defp has_required_keys(_, _), do: false

  def is_authorized(req, state) do
    {auth_user, _} = :cowboy_req.header("x-auth-user", req, nil)
    {auth_token, _} = :cowboy_req.header("x-auth-token", req, nil)
    {user_id, _} = :cowboy_req.binding(:user_id, req, nil)

    if check_token(auth_user, auth_token) && auth_user === user_id do
      {true, req, state}
    else
      msg = "Authorization headers missing or authorization failed."
      {{false, "API Token"}, set_response_text(req, msg), state}
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
