defmodule Wocky.Notifier.InBand.Notification do
  @moduledoc "An invitation from a user to subscribe to a bot"

  use Wocky.Repo.Schema

  import EctoEnum
  import Ecto.Query

  alias Ecto.Queryable
  alias Wocky.Account.User
  alias Wocky.POI.Bot
  alias Wocky.Repo

  defenum(GeofenceEventTypeEnum, :geofence_event_type, [:enter, :exit])

  defenum(NotificationTypeEnum, :notification_type, [
    :bot_invitation,
    :bot_invitation_response,
    :bot_item,
    :geofence_event,
    :location_share,
    :location_share_end,
    :location_share_end_self,
    :user_invitation,
    :user_proximity
  ])

  @foreign_key_type :binary_id
  schema "notifications" do
    field :type, NotificationTypeEnum, null: false
    field :geofence_event, GeofenceEventTypeEnum
    field :bot_invitation_accepted, :boolean
    field :expires_at, :utc_datetime

    belongs_to :user, User
    belongs_to :other_user, User
    belongs_to :bot, Bot
    belongs_to :bot_item, Wocky.POI.Item
    belongs_to :bot_invitation, Wocky.Relation.Invitation, type: :integer
    belongs_to :share, Wocky.Location.Share, type: :integer

    timestamps()
  end

  @type id() :: non_neg_integer()

  @type t() :: %__MODULE__{}

  @spec put(map, atom, [atom]) :: {:ok, t()} | {:error, any()}
  def put(params, type, required) do
    params = Map.put(params, :type, type)

    %Notification{}
    |> changeset(params, required)
    |> Repo.insert()
  end

  @spec user_query(
          User.t(),
          id() | nil,
          id() | nil,
          [NotificationTypeEnum.t()] | nil
        ) :: Queryable.t()
  def user_query(user, before_id, after_id, types \\ nil) do
    Notification
    |> where(user_id: ^user.id)
    |> maybe_add_type_filter(types)
    |> maybe_add_before_id(before_id)
    |> maybe_add_after_id(after_id)
  end

  @spec delete(id() | User.t(), User.t()) :: :ok
  def delete(id, requestor) when is_integer(id) do
    Notification
    |> where([i], i.user_id == ^requestor.id and i.id == ^id)
    |> Repo.delete_all()

    :ok
  end

  def delete(%User{} = user, other_user) do
    Notification
    |> where([i], i.user_id == ^user.id and i.other_user_id == ^other_user.id)
    |> Repo.delete_all()

    :ok
  end

  defp maybe_add_before_id(queryable, nil), do: queryable

  defp maybe_add_before_id(queryable, id) do
    queryable
    |> where([n], n.id < ^id)
  end

  defp maybe_add_after_id(queryable, nil), do: queryable

  defp maybe_add_after_id(queryable, id) do
    queryable
    |> where([n], n.id > ^id)
  end

  defp maybe_add_type_filter(queryable, nil), do: queryable

  defp maybe_add_type_filter(queryable, types) do
    queryable
    |> where([n], n.type in ^types)
  end

  defp changeset(struct, params, required) do
    struct
    |> cast(params, [
      :type,
      :user_id,
      :other_user_id,
      :expires_at,
      :share_id,
      :bot_id,
      :bot_item_id,
      :bot_invitation_id,
      :bot_invitation_accepted,
      :geofence_event
    ])
    |> validate_required([:type | required])
    |> foreign_key_constraint(:user_id)
    |> foreign_key_constraint(:other_user_id)
    |> foreign_key_constraint(:bot_id)
    |> foreign_key_constraint(:bot_item_id)
    |> foreign_key_constraint(:bot_invitation_id)
  end
end
