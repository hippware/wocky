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
          token: binary() | nil,
          message_id: binary() | nil,
          payload: map() | nil,
          payload_string: binary() | nil,
          response: binary() | nil,
          details: binary() | nil,
          created_at: DateTime.t() | nil
        }
end
