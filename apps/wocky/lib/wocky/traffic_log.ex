defmodule Wocky.TrafficLog do
  @moduledoc """
  DB interface module for traffic logging
  """

  use Wocky.Repo.Model

  alias Timex.Duration
  alias Wocky.JID
  alias Wocky.User

  alias __MODULE__, as: TrafficLog

  @foreign_key_type :binary_id
  schema "traffic_logs" do
    field :resource, :binary
    field :ip,       :binary
    field :incoming, :boolean
    field :packet,   :binary

    belongs_to :user, User

    timestamps(updated_at: false)
  end

  @type ip     :: binary
  @type packet :: binary

  @type t :: %TrafficLog{
    user_id:    User.id,
    resource:   JID.resource,
    ip:         ip,
    incoming:   boolean,
    packet:     packet,
    created_at: DateTime.t
  }

  @change_fields [:user_id, :resource, :ip, :packet, :incoming]

  @doc "Write a packet record to the database"
  @spec put(User.id, JID.resource, ip, packet, boolean) :: {:ok, TrafficLog.t}
  def put(user_id, resource, ip, packet, incoming) do
    fields = %{
      user_id: user_id,
      resource: resource,
      ip: ip,
      packet: packet,
      incoming: incoming
    }
    {:ok,
     %TrafficLog{}
     |> cast(fields, @change_fields)
     |> Repo.insert!}
  end

  @spec get_by_period(User.id, DateTime.t, Duration.t) :: [t]
  def get_by_period(user_id, start, duration) do
    TrafficLog
    |> users_traffic(user_id, start, duration)
    |> Repo.all
  end

  @spec get_by_resource(User.id, JID.resource, DateTime.t, Duration.t) :: [t]
  def get_by_resource(user_id, resource, start, duration) do
    TrafficLog
    |> resource_traffic(user_id, resource, start, duration)
    |> Repo.all
  end

  defp users_traffic(query, user_id, startt, duration) do
    endt = Timex.add(startt, duration)
    from t in query, where: t.user_id == ^user_id,
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
