defmodule Wocky.Notifier.InBand.Notification do
  @moduledoc "An invitation from a user to subscribe to a bot"

  use Wocky.Repo.Schema

  import EctoEnum

  alias Wocky.Account.User
  alias Wocky.Contacts.Relationship
  alias Wocky.POI.Bot

  defenum(GeofenceEventTypeEnum, :geofence_event_type, [:enter, :exit])

  defenum(NotificationTypeEnum, :notification_type, [
    :bot_invitation,
    :bot_invitation_response,
    :bot_item,
    :geofence_event,
    :location_share,
    :location_share_end,
    :location_share_end_self,
    :location_share_nearby_start,
    :user_befriend,
    :user_invitation,
    :user_proximity
  ])

  @foreign_key_type :binary_id
  schema "notifications" do
    field :type, NotificationTypeEnum, null: false
    field :data, :map, null: false

    field :geofence_event, GeofenceEventTypeEnum, virtual: true
    field :bot_invitation_accepted, :boolean, virtual: true
    field :expires_at, :utc_datetime, virtual: true
    field :share_id, :integer, virtual: true
    field :share_type, Relationship.LocationShareTypeEnum, virtual: true

    field :other_user_share_type, Relationship.LocationShareTypeEnum,
      virtual: true

    belongs_to :user, User
    belongs_to :other_user, User
    belongs_to :bot, Bot
    belongs_to :bot_item, Wocky.POI.Item
    belongs_to :bot_invitation, Wocky.Relation.Invitation, type: :integer

    timestamps()
  end

  @virtual_fields [
    {:geofence_event, :atom},
    {:bot_invitation_accepted, :atom},
    {:expires_at, :utc_datetime},
    {:share_id, :integer},
    {:share_type, :atom},
    {:other_user_share_type, :atom}
  ]

  @virtual_field_names Enum.map(@virtual_fields, &elem(&1, 0))

  @type id :: non_neg_integer()
  @type t :: %__MODULE__{}

  @spec changeset(t(), map(), [atom()]) :: Changeset.t()
  def changeset(struct, params, required) do
    params = pack_virtual_fields(params)

    {required_virtual, required_real} =
      Enum.split_with([:type, :data | required], fn f ->
        Enum.member?(@virtual_field_names, f)
      end)

    struct
    |> cast(params, [
      :type,
      :user_id,
      :other_user_id,
      :bot_id,
      :bot_item_id,
      :bot_invitation_id,
      :data
    ])
    |> validate_required(required_real)
    |> validate_required_virtual(required_virtual)
    |> foreign_key_constraint(:user_id)
    |> foreign_key_constraint(:other_user_id)
    |> foreign_key_constraint(:bot_id)
    |> foreign_key_constraint(:bot_item_id)
    |> foreign_key_constraint(:bot_invitation_id)
  end

  @spec pack_virtual_fields(map()) :: map()
  def pack_virtual_fields(params) do
    data =
      Enum.reduce(@virtual_fields, %{}, fn {f, t}, d ->
        case Map.get(params, f) do
          nil -> d
          val -> Map.put(d, to_string(f), to_map_type(val, t))
        end
      end)

    Map.put(params, :data, data)
  end

  @spec populate_virtual_fields(t()) :: t()
  def populate_virtual_fields(notification) do
    Enum.reduce(@virtual_fields, notification, fn {f, t}, n ->
      Map.put(n, f, to_native_type(n.data[to_string(f)], t))
    end)
  end

  defp validate_required_virtual(changeset, required) do
    data = Changeset.fetch_change!(changeset, :data)

    Enum.reduce(required, changeset, fn r, c ->
      case Map.get(data, to_string(r)) do
        nil -> Changeset.add_error(c, r, "can't be blank")
        _ -> c
      end
    end)
  end

  defp to_native_type(nil, _), do: nil
  defp to_native_type(val, :atom), do: String.to_existing_atom(val)
  defp to_native_type(val, :integer), do: val

  defp to_native_type(val, :utc_datetime) do
    {:ok, val, 0} = DateTime.from_iso8601(val)
    val
  end

  defp to_map_type(val, :atom), do: to_string(val)
  defp to_map_type(val, :integer), do: val
  defp to_map_type(val, :utc_datetime), do: DateTime.to_iso8601(val)
end
