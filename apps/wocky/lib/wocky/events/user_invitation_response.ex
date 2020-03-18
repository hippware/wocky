# credo:disable-for-this-file Credo.Check.Readability.StrictModuleLayout
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

  @impl true
  def notify?(_), do: true

  @impl true
  def recipient(%{to: to}), do: to

  @impl true
  def message(%{from: from} = _event) do
    get_handle(from) <> " connected with you"
  end

  @impl true
  def uri(%{from: from} = _event), do: make_uri(:user, from.id)

  @impl true
  def ignore_block?(_event), do: false

  @impl true
  def opts(_), do: []
end
