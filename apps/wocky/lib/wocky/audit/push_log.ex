defmodule Wocky.Audit.PushLog do
  @moduledoc "Record for push notification logs"

  alias Wocky.Account.User

  defstruct [
    :user_id,
    :device,
    :token,
    :message_id,
    :payload,
    :payload_string,
    :response,
    :details,
    :created_at
  ]

  @type t :: %__MODULE__{
          user_id: User.id() | nil,
          device: User.device() | nil,
          token: String.t() | nil,
          message_id: String.t() | nil,
          payload: map() | nil,
          payload_string: String.t() | nil,
          response: String.t() | nil,
          details: String.t() | nil,
          created_at: DateTime.t() | nil
        }
end
