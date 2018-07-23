defmodule WockyAPI.PipelineLog do
  @moduledoc """
  "Module to provide wocky traffic logging as an Absinthe Pipeline phase
  """

  alias Wocky.TrafficLog
  alias Wocky.User

  def run(blueprint, opts) do
    context = opts[:context]

    user_id =
      case context[:current_user] do
        %User{id: id} -> id
        _ -> nil
      end

    case opts[:phase] do
      :request ->
        log(request(blueprint, opts[:variables]), user_id, context, false)

      :response ->
        log(result(blueprint.result), user_id, context, true)
    end

    {:ok, blueprint}
  end

  defp request(query, variables),
    do: query <> " / " <> inspect(variables)

  defp result(result),
    do: inspect(result[:data]) <> " / " <> inspect(result[:errors])

  defp log(nil, _user_id, _context, _incoming), do: :ok

  defp log(packet, user_id, context, incoming) do
    %{
      user_id: user_id,
      resource: "GraphQL",
      host: context.host,
      ip: context.peer,
      incoming: incoming,
      packet: packet
    }
    |> TrafficLog.put()
  end
end
