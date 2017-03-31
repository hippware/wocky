defmodule Wocky.PushNotifier.SNS do
  @moduledoc """
  Implementes the `PushNotifier` behavior for Amazon SNS.
  """

  alias ExAws.SNS

  require Logger

  @behaviour Wocky.PushNotifier

  @message_limit 512

  @spec init :: :ok
  def init, do: :ok

  @spec register(binary, binary, binary, binary) :: {:ok, binary} | {:error, any}
  def register(user_id, resource, _platform, device_id) do
    :wocky
    |> Application.fetch_env!(:application_arn)
    |> SNS.create_platform_endpoint(device_id, "#{user_id}/#{resource}")
    |> ExAws.request
    |> handle_register_result
  end

  defp handle_register_result({:error, error}), do: handle_aws_error(error)
  defp handle_register_result({:ok, %{body: %{endpoint_arn: arn} = body}}) do
    :ok = Logger.debug("SNS register response:\n#{inspect(body)}")
    {:ok, arn}
  end

  @spec push(binary, binary) :: :ok | {:error, any}
  def push(endpoint, message) do
    message
    |> maybe_truncate_message
    |> make_payload
    |> SNS.publish([target_arn: endpoint, message_structure: :json])
    |> ExAws.request
    |> handle_push_result
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

  defp handle_push_result({:error, error}), do: handle_aws_error(error)
  defp handle_push_result({:ok, %{body: body}}) do
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
