defmodule Wocky.Notification.AWSHandler do
  @moduledoc """
  Implementes the `wocky_notification_handler` behavior for AWS. All
  notifications will be sent through SNS.
  """

  alias ExAws.SNS
  require Logger

  @behaviour :wocky_notification_handler

  @message_limit 512

  @spec register(binary, binary, binary) :: {:ok, binary} | {:error, any}
  def register(user, _platform, device_id) do
    user_data = user |> :jid.from_binary |> :jid.to_bare |> :jid.to_binary

    :application_arn
    |> :wocky_app.get_config("")
    |> SNS.create_platform_endpoint(device_id, user_data)
    |> ExAws.request
    |> handle_register_result
  end

  defp handle_register_result({:error, error}), do: handle_aws_error(error)
  defp handle_register_result({:ok, %{body: body}}) do
    :ok = Logger.debug("SNS register response:\n#{body}")

    {:ok, xml} = :exml.parse(body)
    arn = :exml_query.path(xml, [
            {:element, "CreatePlatformEndpointResult"},
            {:element, "EndpointArn"},
            :cdata
          ])

    {:ok, arn}
  end

  @spec notify(binary, binary) :: :ok | {:error, any}
  def notify(endpoint, message) do
    message
    |> maybe_truncate_message
    |> make_payload
    |> SNS.publish([target_arn: endpoint, message_structure: :json])
    |> ExAws.request
    |> handle_notify_result
  end

  @spec notify_message(binary, binary, binary) :: :ok | {:error, any}
  def notify_message(endpoint, from, body) do
    body
    |> format_message(from)
    |> maybe_truncate_message
    |> make_payload
    |> SNS.publish([target_arn: endpoint, message_structure: :json])
    |> ExAws.request
    |> handle_notify_result
  end

  defp format_message(body, from) do
    "From #{from}:\n#{body}"
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
    :ok = Logger.debug("SNS notification response:\n#{body}")
    :ok
  end

  defp handle_aws_error({:http_error, code, %{body: body}} = error) do
    :ok = Logger.error("SNS API error (#{code}): #{body}")
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
