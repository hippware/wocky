defmodule Wocky.Audit.TrafficLog do
  @moduledoc """
  DB interface module for traffic logging
  """

  use Wocky.Repo.Schema

  import Ecto.Changeset

  alias Wocky.Account.User

  @foreign_key_type :binary_id
  schema "traffic_logs" do
    field :device, :binary
    field :host, :binary
    field :ip, :binary, default: ""
    field :incoming, :boolean
    field :packet, :binary

    belongs_to :user, User

    timestamps(updated_at: false)
  end

  @type ip :: binary
  @type packet :: binary

  @type t :: %TrafficLog{
          user_id: User.id(),
          device: User.device(),
          host: binary,
          ip: ip,
          incoming: boolean,
          packet: packet,
          created_at: DateTime.t()
        }

  @change_fields [:user_id, :device, :host, :ip, :incoming, :packet]
  @required_fields [:device, :host, :ip, :incoming, :packet]

  @doc false
  def changeset(fields) do
    %TrafficLog{}
    |> cast(fields, @change_fields)
    |> validate_required(@required_fields)
    |> foreign_key_constraint(:user_id)
  end
end
