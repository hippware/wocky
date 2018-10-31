defmodule Wocky.TrafficLog do
  @moduledoc """
  DB interface module for traffic logging
  """

  use Wocky.Repo.Schema

  import Ecto.Query

  alias Ecto.Changeset
  alias Timex.Duration
  alias Wocky.JID
  alias Wocky.Repo
  alias Wocky.User
  alias __MODULE__

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

  @doc "Write a packet record to the database"
  @spec put(map) :: {:ok, TrafficLog.t()} | {:error, Changeset.t()}
  def put(fields) do
    %TrafficLog{}
    |> cast(fields, @change_fields)
    |> validate_required(@required_fields)
    |> foreign_key_constraint(:user_id)
    |> Repo.insert()
  end

  @spec get_by_period(User.id(), DateTime.t(), Duration.t()) :: [t]
  def get_by_period(user_id, start, duration) do
    TrafficLog
    |> users_traffic(user_id, start, duration)
    |> Repo.all()
  end

  @spec get_by_device(User.id(), JID.device(), DateTime.t(), Duration.t()) ::
          [t]
  def get_by_device(user_id, device, start, duration) do
    TrafficLog
    |> device_traffic(user_id, device, start, duration)
    |> Repo.all()
  end

  defp users_traffic(query, user_id, startt, duration) do
    endt = Timex.add(startt, duration)

    from t in query,
      where: t.user_id == ^user_id,
      where: t.created_at >= ^startt,
      where: t.created_at <= ^endt,
      select: t,
      order_by: [asc: :created_at]
  end

  defp device_traffic(query, user_id, device, startt, duration) do
    query = users_traffic(query, user_id, startt, duration)
    from t in query, where: t.device == ^device
  end
end
