defmodule Wocky.User.Notification do
  @moduledoc "An invitation from a user to subscribe to a bot"

  use Wocky.Repo.Schema

  import EctoHomoiconicEnum, only: [defenum: 2]
  import Ecto.Query

  alias Ecto.Queryable

  alias Wocky.{Block, Bot, Repo, User}

  alias Wocky.User.Notification.{
    BotInvitation,
    BotInvitationResponse,
    BotItem,
    GeofenceEvent,
    LocationShare,
    LocationShareEnd,
    UserInvitation
  }

  alias Wocky.User.Notifier

  alias __MODULE__, as: Notification

  defenum NotificationType, [
    :bot_invitation,
    :bot_invitation_response,
    :bot_item,
    :geofence_event,
    :location_share,
    :location_share_end,
    :user_invitation
  ]

  @foreign_key_type :binary_id
  schema "notifications" do
    field :type, NotificationType, null: false
    field :geofence_event, GeofenceEvent.GeofenceEventType
    field :bot_invitation_accepted, :boolean

    belongs_to :user, User
    belongs_to :other_user, User
    belongs_to :bot, Bot
    belongs_to :bot_item, Bot.Item
    belongs_to :bot_invitation, Bot.Invitation, type: :integer

    timestamps()
  end

  @type id() :: non_neg_integer()

  @type t() ::
          BotInvitation.t()
          | BotInvitationResponse.t()
          | BotItem.t()
          | GeofenceEvent.t()
          | LocationShare.t()
          | LocationShareEnd.t()
          | UserInvitation.t()

  @type base() :: %__MODULE__{}

  @spec notify(t()) :: {:ok, t()} | {:error, any()}
  def notify(notification) do
    if Block.blocked?(notification.user_id, notification.other_user_id) do
      {:error, :invalid_user}
    else
      Notifier.notify(notification)
    end
  end

  @spec decode(base()) :: t()
  def decode(%Notification{type: type} = params) do
    struct =
      case type do
        :bot_invitation -> %BotInvitation{}
        :bot_invitation_response -> %BotInvitationResponse{}
        :bot_item -> %BotItem{}
        :geofence_event -> %GeofenceEvent{}
        :location_share -> %LocationShare{}
        :location_share_end -> %LocationShareEnd{}
        :user_invitation -> %UserInvitation{}
      end

    Notifier.decode(struct, params)
  end

  @spec put(map, atom, [atom]) :: {:ok, base()} | {:error, any()}
  def put(params, type, required) do
    params =
      params
      |> Map.from_struct()
      |> Map.put(:type, type)

    %Notification{}
    |> changeset(params, required)
    |> Repo.insert()
  end

  @spec user_query(
          User.t(),
          id() | nil,
          id() | nil,
          [NotificationType.t()] | nil
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
