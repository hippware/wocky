defmodule WockyAPI.Factory do
  @moduledoc """
  Factory for Ecto-persisted structures
  """

  use ExMachina.Ecto, repo: Wocky.Repo

  alias Wocky.Account.JWT.Client, as: ClientJWT
  alias Wocky.Account.JWT.Server, as: ServerJWT
  alias Wocky.Notifier.InBand.Notification
  alias Wocky.Repo.Factory, as: RepoFactory
  alias Wocky.Repo.Timestamp
  alias Wocky.TROS

  def bot_invitation_notification_factory do
    inviter = RepoFactory.build(:user)
    bot = RepoFactory.build(:bot, user: inviter)

    %Notification{
      type: :bot_invitation,
      user: RepoFactory.build(:user),
      other_user: inviter,
      bot: bot
    }
  end

  def bot_invitation_response_notification_factory do
    %{
      bot_invitation_notification_factory()
      | type: :bot_invitation_response,
        bot_invitation_accepted: true
    }
  end

  def bot_item_notification_factory do
    owner = RepoFactory.build(:user)
    bot = RepoFactory.build(:bot, user: owner)

    %Notification{
      type: :bot_item,
      user: owner,
      other_user: RepoFactory.build(:user),
      bot: bot,
      bot_item: RepoFactory.build(:item)
    }
  end

  def geofence_event_notification_factory do
    %Notification{
      type: :geofence_event,
      user: RepoFactory.build(:user),
      other_user: RepoFactory.build(:user),
      bot: RepoFactory.build(:bot),
      geofence_event: :enter
    }
  end

  def location_share_notification_factory do
    %Notification{
      type: :location_share,
      user: RepoFactory.build(:user),
      other_user: RepoFactory.build(:user),
      expires_at: Timestamp.shift(days: 1)
    }
  end

  def user_invitation_notification_factory do
    %Notification{
      type: :user_invitation,
      user: RepoFactory.build(:user),
      other_user: RepoFactory.build(:user)
    }
  end

  def user_proximity_notification_factory do
    %Notification{
      type: :user_proximity,
      user: RepoFactory.build(:user),
      other_user: RepoFactory.build(:user)
    }
  end

  def image_url(image), do: TROS.make_url(image.id)

  def get_test_token(user, claims \\ %{}) do
    {:ok, token, _} = ClientJWT.encode_and_sign(user, claims)
    token
  end

  def get_test_location_token(user) do
    {:ok, token, _} = ServerJWT.encode_and_sign(user)
    token
  end
end
