defmodule Wocky.Notifier.Push.Utils do
  @moduledoc """
  Utility functions to help with push notifications
  """

  alias Wocky.Account.User
  alias Wocky.Notifier.Push
  alias Wocky.POI.Bot

  @message_limit 512

  def maybe_truncate_message(message) do
    if byte_size(message) > @message_limit do
      String.slice(message, 0, @message_limit - 3) <> "..."
    else
      message
    end
  end

  @doc false
  def blank?(nil), do: true
  def blank?(""), do: true
  def blank?(_), do: false

  @doc false
  def get_handle(obj) do
    case do_get_handle(obj) do
      nil -> "Someone"
      "" -> "Someone"
      handle -> "@" <> handle
    end
  end

  defp do_get_handle(nil), do: nil
  defp do_get_handle(%User{} = user), do: user.handle

  @doc false
  def get_title(obj) do
    case do_get_title(obj) do
      nil -> "Somewhere"
      "" -> "Somewhere"
      title -> title
    end
  end

  defp do_get_title(nil), do: nil
  defp do_get_title(%Bot{} = bot), do: bot.title

  @doc false
  def make_uri(type, id \\ nil, server? \\ true, suffix \\ "") do
    "#{uri_prefix()}://#{type}"
    |> maybe_add_server(server?)
    |> maybe_append(id)
    |> maybe_append(suffix)
  end

  defp maybe_add_server(uri, false), do: uri
  defp maybe_add_server(uri, true), do: uri <> "/" <> server()

  defp maybe_append(uri, nil), do: uri
  defp maybe_append(uri, id), do: uri <> "/" <> id

  defp server, do: Confex.get_env(:wocky, :wocky_host)

  defp uri_prefix, do: Push.get_config(:uri_prefix)
end
