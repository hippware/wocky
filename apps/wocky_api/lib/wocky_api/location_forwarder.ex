defmodule WockyAPI.LocationForwarder do
  @moduledoc """
  Module to optionally forward received locations to an external HTTP target.
  Only locations that would be audited are forwarded.
  """

  alias Wocky.Account.User

  def forward(user_id, l) do
    target = Confex.get_env(:wocky_api, :location_forward_target)

    if target do
      user = User.hydrate(user_id)

      if should_forward?(user) do
        do_forward(user, l, target)
      end
    end
  end

  defp should_forward?(user) do
    FunWithFlags.Group.in?(user, :hippware) ||
      FunWithFlags.enabled?(:location, for: user)
  end

  @headers [{"Content-Type", "application/json"}]

  defp do_forward(user, location, target) do
    payload = %{location: location} |> Poison.encode!()
    url = target <> user.handle
    HTTPoison.post(url, payload, @headers)
  end
end
