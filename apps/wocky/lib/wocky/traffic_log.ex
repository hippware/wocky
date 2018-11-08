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
    field :resource, :binary
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
          resource: User.resource(),
          host: binary,
          ip: ip,
          incoming: boolean,
          packet: packet,
          created_at: DateTime.t()
        }

  @change_fields [:user_id, :resource, :host, :ip, :incoming, :packet]
  @required_fields [:resource, :host, :ip, :incoming, :packet]

  @doc "Write a packet record to the database"
  @spec put(map, boolean) :: {:ok, TrafficLog.t() | nil} | {:error, Changeset.t()}
  def put(fields, force? \\ false) do
    if force? || Confex.get_env(:wocky, :log_traffic) do
      %TrafficLog{}
      |> cast(fields, @change_fields)
      |> validate_required(@required_fields)
      |> foreign_key_constraint(:user_id)
      |> Repo.insert()
    else
      {:ok, nil}
    end
  end

  @spec get_by_period(User.id(), DateTime.t(), Duration.t()) :: [t]
  def get_by_period(user_id, start, duration) do
    TrafficLog
    |> users_traffic(user_id, start, duration)
    |> Repo.all()
  end

  @spec get_by_resource(User.id(), JID.resource(), DateTime.t(), Duration.t()) ::
          [t]
  def get_by_resource(user_id, resource, start, duration) do
    TrafficLog
    |> resource_traffic(user_id, resource, start, duration)
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

  defp resource_traffic(query, user_id, resource, startt, duration) do
    query = users_traffic(query, user_id, startt, duration)
    from t in query, where: t.resource == ^resource
  end
end
