defmodule WockyAPI.Factory do
  @moduledoc """
  Factory for Ecto-persisted structures
  """

  use ExMachina.Ecto, repo: Wocky.Repo

  alias Wocky.Account.JWT.Client, as: ClientJWT
  alias Wocky.Account.JWT.Server, as: ServerJWT
  alias Wocky.Account.User
  alias Wocky.Notifier.InBand.Notification
  alias Wocky.Repo.Factory, as: RepoFactory
  alias Wocky.Repo.Timestamp
  alias Wocky.TROS

  @spec bot_invitation_notification_factory(map()) :: %Notification{}
  def bot_invitation_notification_factory(attrs) do
    inviter = RepoFactory.build(:user)
    bot = RepoFactory.build(:bot, user: inviter)

    %Notification{
      type: :bot_invitation,
      user: RepoFactory.build(:user),
      other_user: inviter,
      bot: bot
    }
    |> merge_attributes(attrs)
    |> Notification.pack_virtual_fields()
  end

  @spec bot_invitation_response_notification_factory(map()) :: %Notification{}
  def bot_invitation_response_notification_factory(attrs) do
    %{
      bot_invitation_notification_factory(attrs)
      | type: :bot_invitation_response,
        bot_invitation_accepted: true
    }
    |> merge_attributes(attrs)
    |> Notification.pack_virtual_fields()
  end

  @spec bot_item_notification_factory(map()) :: %Notification{}
  def bot_item_notification_factory(attrs) do
    owner = RepoFactory.build(:user)
    bot = RepoFactory.build(:bot, user: owner)

    %Notification{
      type: :bot_item,
      user: owner,
      other_user: RepoFactory.build(:user),
      bot: bot,
      bot_item: RepoFactory.build(:item)
    }
    |> merge_attributes(attrs)
    |> Notification.pack_virtual_fields()
  end

  @spec geofence_event_notification_factory(map()) :: %Notification{}
  def geofence_event_notification_factory(attrs) do
    %Notification{
      type: :geofence_event,
      user: RepoFactory.build(:user),
      other_user: RepoFactory.build(:user),
      bot: RepoFactory.build(:bot),
      geofence_event: :enter
    }
    |> merge_attributes(attrs)
    |> Notification.pack_virtual_fields()
  end

  @spec location_share_notification_factory(map()) :: %Notification{}
  def location_share_notification_factory(attrs) do
    %Notification{
      type: :location_share,
      user: RepoFactory.build(:user),
      other_user: RepoFactory.build(:user),
      expires_at: Timestamp.shift(days: 1),
      share_id: 0
    }
    |> merge_attributes(attrs)
    |> Notification.pack_virtual_fields()
  end

  @spec user_invitation_notification_factory(map()) :: %Notification{}
  def user_invitation_notification_factory(attrs) do
    %Notification{
      type: :user_invitation,
      user: RepoFactory.build(:user),
      other_user: RepoFactory.build(:user)
    }
    |> merge_attributes(attrs)
    |> Notification.pack_virtual_fields()
  end

  @spec image_url(TROS.Metadata.t()) :: TROS.url()
  def image_url(image), do: TROS.make_url(image.id)

  @spec get_test_token(User.t(), map()) :: String.t()
  def get_test_token(user, claims \\ %{}) do
    {:ok, token, _} = ClientJWT.encode_and_sign(user, claims)
    token
  end

  @spec get_test_location_token(User.t()) :: String.t()
  def get_test_location_token(user) do
    {:ok, token, _} = ServerJWT.encode_and_sign(user)
    token
  end
end
