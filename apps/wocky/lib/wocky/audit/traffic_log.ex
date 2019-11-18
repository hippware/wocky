defmodule Wocky.Audit.TrafficLog do
  @moduledoc """
  Record for traffic logging
  """

  alias Wocky.Account.User

  defstruct [
    :user_id,
    :device,
    :host,
    :ip,
    :incoming,
    :packet,
    :created_at
  ]

  @type ip :: String.t()
  @type packet :: String.t()

  @type t :: %__MODULE__{
          user_id: User.id(),
          device: User.device(),
          host: String.t(),
          ip: ip(),
          incoming: boolean(),
          packet: packet(),
          created_at: DateTime.t()
        }
end
