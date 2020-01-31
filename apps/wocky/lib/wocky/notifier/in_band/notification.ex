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
    :user_invitation,
    :user_proximity
  ])

  @foreign_key_type :binary_id
  schema "notifications" do
    field :type, NotificationTypeEnum, null: false
    field :geofence_event, GeofenceEventTypeEnum
    field :share_type, Relationship.LocationShareTypeEnum
    field :bot_invitation_accepted, :boolean
    field :expires_at, :utc_datetime

    belongs_to :user, User
    belongs_to :other_user, User
    belongs_to :bot, Bot
    belongs_to :bot_item, Wocky.POI.Item
    belongs_to :bot_invitation, Wocky.Relation.Invitation, type: :integer
    belongs_to :share, Relationship, type: :integer

    timestamps()
  end

  @type id :: non_neg_integer()
  @type t :: %__MODULE__{}

  @spec changeset(t(), map(), [atom()]) :: Changeset.t()
  def changeset(struct, params, required) do
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
      :geofence_event,
      :share_type
    ])
    |> validate_required([:type | required])
    |> foreign_key_constraint(:user_id)
    |> foreign_key_constraint(:other_user_id)
    |> foreign_key_constraint(:bot_id)
    |> foreign_key_constraint(:bot_item_id)
    |> foreign_key_constraint(:bot_invitation_id)
  end
end
