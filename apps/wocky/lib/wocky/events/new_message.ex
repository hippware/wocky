defmodule Wocky.Events.NewMessage do
  @moduledoc false

  alias Wocky.Account.User

  defstruct [
    :to,
    :from,
    :content,
    :image_url,
    :conversation_id
  ]

  @type t :: %__MODULE__{
          to: User.t(),
          from: User.t(),
          content: nil | binary,
          image_url: nil | binary,
          conversation_id: binary
        }

  use ExConstructor
end

defimpl Wocky.Notifier.Push.Event, for: Wocky.Events.NewMessage do
  import Wocky.Notifier.Push.Utils

  def notify?(_), do: true

  def recipient(%{to: to}), do: to

  def message(%{from: from, content: content}) do
    if blank?(content) do
      get_handle(from) <> " sent you an image"
    else
      "From: #{get_handle(from)}\n#{content}"
    end
  end

  def uri(%{from: from}), do: make_uri(:conversation, from.id)

  def opts(_), do: [sound: "default"]
end
