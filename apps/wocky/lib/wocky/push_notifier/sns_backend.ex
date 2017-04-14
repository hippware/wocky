defmodule Wocky.PushNotifier.SNSBackend do
  @moduledoc """
  Implementes the `PushNotifier` behavior for Amazon SNS.
  """

  use Wocky.JID

  alias ExAws.SNS

  require Logger

  @behaviour Wocky.PushNotifier

  @message_limit 512

  def init, do: :ok

  def enable(user, server, resource, _platform, device_id) do
    user_data = user |> JID.make(server, resource) |> JID.to_binary

    :wocky
    |> Application.fetch_env!(:application_arn)
    |> SNS.create_platform_endpoint(device_id, user_data)
    |> ExAws.request
    |> handle_register_result
  end

  defp handle_register_result({:error, error}), do: handle_aws_error(error)
  defp handle_register_result({:ok, %{body: %{endpoint_arn: arn} = body}}) do
    :ok = Logger.debug("SNS register response:\n#{inspect(body)}")
    {:ok, arn}
  end

  def disable(_endpoint) do
    # TODO delete the endpoint when disable is called.
    :ok
  end

  def push(body, endpoint) do
    body
    |> maybe_truncate_message
    |> make_payload
    |> SNS.publish([target_arn: endpoint, message_structure: :json])
    |> ExAws.request
    |> handle_notify_result
  end

  defp maybe_truncate_message(message) do
    if byte_size(message) > @message_limit do
      String.slice(message, 0, @message_limit - 3) <> "..."
    else
      message
    end
  end

  defp make_payload(message) do
    Poison.encode!(%{
      default: message,
      'APNS': Poison.encode!(%{
        aps: %{
          alert: message,
          badge: 1
        }
      })
    })
  end

  defp handle_notify_result({:error, error}), do: handle_aws_error(error)
  defp handle_notify_result({:ok, %{body: body}}) do
    :ok = Logger.debug("SNS notification response:\n#{inspect(body)}")
    :ok
  end

  defp handle_aws_error({:http_error, code, %{body: body}} = error) do
    :ok = Logger.error("SNS API error (#{code}): #{inspect(body)}")
    {:error, error}
  end

  defp handle_aws_error({:http_error, code, body} = error) do
    :ok = Logger.error("SNS API error (#{code}): #{inspect(body)}")
    {:error, error}
  end

  defp handle_aws_error(error) do
    :ok = Logger.error("SNS API error: #{inspect(error)}")
    {:error, error}
  end
end
