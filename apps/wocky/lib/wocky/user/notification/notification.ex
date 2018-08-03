defmodule Wocky.User.Notification do
  @moduledoc "An invitation from a user to subscribe to a bot"

  use Wocky.Repo.Schema

  import EctoHomoiconicEnum, only: [defenum: 2]
  import Ecto.Query

  alias Wocky.{Bot, Repo, User}

  alias Wocky.User.Notification.{
    BotItem,
    GeofenceEvent,
    Invitation,
    InvitationResponse,
    UserFollow
  }

  alias Wocky.User.Notifier

  alias __MODULE__, as: Notification

  defenum NotificationType, [
    :bot_item,
    :geofence_event,
    :invitation,
    :invitation_response,
    :user_follow
  ]

  @foreign_key_type :binary_id
  schema "notifications" do
    field :type, NotificationType, null: false
    field :geofence_event, GeofenceEvent.GeofenceEventType
    field :invitation_accepted, :boolean

    belongs_to :user, User
    belongs_to :other_user, User
    belongs_to :bot, Bot
    belongs_to :bot_item, Bot.Item
    belongs_to :invitation, Bot.Invitation, type: :integer

    timestamps()
  end

  @type t() ::
          BotItem.t()
          | GeofenceEvent.t()
          | Invitation.t()
          | InvitationResponse.t()
          | UserFollow.t()

  @type base() :: %__MODULE__{}

  @spec notify(t()) :: {:ok, t()} | {:error, any()}
  def notify(notification), do: Notifier.notify(notification)

  @spec decode(base()) :: t()
  def decode(%Notification{type: type} = params) do
    struct =
      case type do
        :bot_item -> %BotItem{}
        :geofence_event -> %GeofenceEvent{}
        :invitation -> %Invitation{}
        :invitation_response -> %InvitationResponse{}
        :user_follow -> %UserFollow{}
      end

    Notifier.decode(struct, params)
  end

  @spec put(t(), NotificationType, [atom]) :: {:ok, base()} | {:error, any()}
  def put(params, type, required) do
    params =
      params
      |> Map.from_struct()
      |> Map.put(:type, type)

    %Notification{}
    |> changeset(params, required)
    |> Repo.insert()
  end

  @spec user_query(User.t()) :: Queryable.t()
  def user_query(user) do
    Notification
    |> where(user_id: ^user.id)
  end

  defp changeset(struct, params, required) do
    struct
    |> cast(params, [
      :type,
      :user_id,
      :other_user_id,
      :bot_id,
      :bot_item_id,
      :invitation_id,
      :invitation_accepted,
      :geofence_event
    ])
    |> validate_required([:type | required])
    |> foreign_key_constraint(:user_id)
    |> foreign_key_constraint(:other_user_id)
    |> foreign_key_constraint(:bot_id)
    |> foreign_key_constraint(:bot_item_id)
    |> foreign_key_constraint(:invitation_id)
  end
end
