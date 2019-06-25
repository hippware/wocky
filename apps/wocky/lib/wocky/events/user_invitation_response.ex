defmodule Wocky.Events.UserInvitationResponse do
  @moduledoc "Notification for a user accepting a friend invitation"

  alias Wocky.Account.User

  defstruct [
    :to,
    :from
  ]

  @type t :: %__MODULE__{
          to: User.t(),
          from: User.t()
        }

  use ExConstructor
end

defimpl Wocky.Notifier.Push.Event, for: Wocky.Events.UserInvitationResponse do
  import Wocky.Notifier.Push.Utils

  def notify?(_), do: true

  def recipient(%{to: to}), do: to

  def message(%{from: from} = _event) do
    get_handle(from) <> " connected with you"
  end

  def uri(%{from: from} = _event), do: make_uri(:user, from.id)

  def opts(_), do: []
end
