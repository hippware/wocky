defmodule WockyAPI.PipelineLog do
  @moduledoc """
  "Module to provide wocky traffic logging as an Absinthe Pipeline phase
  """

  alias Wocky.TrafficLog
  alias Wocky.User

  def run(blueprint, opts) do
    user_id =
      case opts[:context][:current_user] do
        %User{id: id} -> id
        _ -> nil
      end

    case opts[:phase] do
      :request -> log(blueprint, user_id, false)
      :response -> log(inspect(blueprint.result.data), user_id, true)
    end

    {:ok, blueprint}
  end

  defp log(nil, _user_id, _incoming), do: :ok

  defp log(packet, user_id, incoming) do
    %{
      user_id: user_id,
      resource: "GraphQL",
      host: host(),
      ip: "",
      incoming: incoming,
      packet: packet
    }
    |> TrafficLog.put()
  end

  defp host() do
    {:ok, host} = :inet.gethostname()
    to_string(host)
  end
end
