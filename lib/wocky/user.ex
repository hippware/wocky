defmodule Wocky.User do
  @moduledoc ""

  alias :wocky_db, as: Db

  @type t :: %__MODULE__{
    user:           binary,
    server:         binary,
    handle:         binary,
    password:       binary,
    avatar:         binary,
    first_name:     binary,
    last_name:      binary,
    email:          binary,
    external_id:    binary,
    phone_number:   binary,
    roster_viewers: [binary]
  }

  @enforce_keys [:user, :server, :handle]
  defstruct [
    user:           nil,
    server:         nil,
    handle:         nil,
    password:       nil,
    avatar:         nil,
    first_name:     nil,
    last_name:      nil,
    email:          nil,
    external_id:    nil,
    phone_number:   nil,
    roster_viewers: []
  ]

  def make_id do
    Db.create_id
  end

  def insert(%__MODULE__{} = struct) do
    {phone_number, user} =
      struct
      |> Map.from_struct
      |> Map.pop(:phone_number)

    :ok = Db.insert(:shared, :user, user)
    :ok = Db.insert(:shared, :phone_number_to_user,
                    %{user: user.user, server: user.server,
                      phone_number: phone_number})
    :ok = Db.insert(:shared, :handle_to_user,
                    %{user: user.user, server: user.server,
                      handle: user.handle})
  end
end
