defmodule WockyAPI.PipelineLog do
  @moduledoc """
  "Module to provide wocky traffic logging as an Absinthe Pipeline phase
  """

  alias Wocky.TrafficLog
  alias Wocky.User

  def run(blueprint, opts) do
    context = opts[:context]

    case opts[:phase] do
      :request ->
        log(request(blueprint, opts[:variables]), context, false)

      :response ->
        log(result(blueprint.result), context, true)
    end

    {:ok, blueprint}
  end

  defp request(query, variables),
    do: query <> " / " <> inspect(variables)

  defp result(result),
    do: inspect(result[:data]) <> " / " <> inspect(result[:errors])

  defp log(packet, %{current_user: %User{} = user} = context, incoming),
    do: log(packet, user.id, context, incoming, User.hippware?(user))

  defp log(packet, context, incoming),
    do: log(packet, nil, context, incoming, false)

  defp log(packet, user_id, context, incoming, force) do
    %{
      user_id: user_id,
      device: "GraphQL",
      host: context.host,
      ip: context.peer,
      incoming: incoming,
      packet: packet
    }
    |> TrafficLog.put(force)
  end
end
