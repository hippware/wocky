defmodule Wocky.Repo.Factory do
  @moduledoc """
  Factory for Ecto-persisted structures
  """

  use ExMachina.Ecto, repo: Wocky.Repo

  alias Faker.Address
  alias Faker.Company
  alias Faker.Internet
  alias Faker.Lorem
  alias Faker.Name
  alias Faker.Phone.EnUs, as: Phone
  alias Faker.String
  alias Wocky.GeoUtils
  alias Wocky.Notifier.InBand.Notification
  alias Wocky.Repo.ID
  alias Wocky.TROS
  alias Wocky.UserInvite.InviteCode

  def user_factory do
    %Wocky.Account.User{
      id: ID.new(),
      external_id: external_id(),
      handle: handle(),
      image_url: TROS.make_url(ID.new()),
      name: Name.name(),
      phone_number: phone_number(),
      email: Internet.email(),
      tagline: Lorem.sentence(),
      provider: "local",
      roles: [],
      welcome_sent: false,
      smss_sent: 0,
      bot_created: false,
      transient: false
    }
  end

  def bot_factory do
    %Wocky.POI.Bot{
      id: ID.new(),
      user: build(:user),
      pending: false,
      title: Company.name(),
      shortname: Lorem.sentence(),
      description: Lorem.paragraph(%Range{first: 1, last: 2}),
      image_url: TROS.make_url(ID.new()),
      type: "test",
      icon: Lorem.word(),
      address: Address.street_address(),
      address_data: "{name: foo}",
      location: GeoUtils.point(Address.latitude(), Address.longitude()),
      radius: :rand.uniform() * 100.0,
      tags: []
    }
  end

  def item_factory do
    %Wocky.POI.Item{
      id: ID.new(),
      bot: build(:bot),
      content: Lorem.sentence()
    }
  end

  def subscription_factory do
    %Wocky.Relation.Subscription{
      user: build(:user),
      bot: build(:bot)
    }
  end

  def message_factory do
    %Wocky.Messaging.Message{
      sender: build(:user),
      recipient: build(:user),
      content: Lorem.paragraph(),
      client_data: Lorem.paragraph(),
      read: false
    }
  end

  def tros_metadata_factory do
    %Wocky.TROS.Metadata{
      id: ID.new(),
      user: build(:user),
      access: Lorem.sentence(),
      ready: true,
      available_formats: [:full, :original, :thumbnail]
    }
  end

  def friend_factory do
    %Wocky.Contacts.Relationship{
      user: build(:user),
      contact: build(:user),
      share_type: :always,
      state: :friend,
      nearby: false
    }
  end

  def location_factory do
    %Wocky.Location.UserLocation{
      user_id: build(:user).id,
      device: device(),
      lat: Address.latitude(),
      lon: Address.longitude(),
      accuracy: 10.0,
      activity: Lorem.word(),
      captured_at: DateTime.utc_now()
    }
  end

  def bot_event_factory do
    %Wocky.Location.BotEvent{
      bot: build(:bot),
      user: build(:user),
      device: device(),
      event: :enter,
      occurred_at: DateTime.utc_now()
    }
  end

  def push_token_factory do
    %Wocky.Notifier.Push.Token{
      user: build(:user),
      device: device(),
      token: ID.new(),
      valid: true,
      enabled_at: DateTime.utc_now()
    }
  end

  def bot_invitation_factory do
    %Wocky.Relation.Invitation{
      user: build(:user),
      invitee: build(:user),
      bot: build(:bot),
      accepted: nil
    }
  end

  @spec bot_invitation_notification_factory(map()) :: Notification.t()
  def bot_invitation_notification_factory(attrs) do
    inviter = build(:user)
    bot = build(:bot, user: inviter)

    %Notification{
      type: :bot_invitation,
      user: build(:user),
      other_user: inviter,
      bot: bot
    }
    |> merge_attributes(attrs)
    |> Notification.pack_virtual_fields()
  end

  @spec geofence_event_notification_factory(map()) :: Notification.t()
  def geofence_event_notification_factory(attrs) do
    %Notification{
      type: :geofence_event,
      user: build(:user),
      other_user: build(:user),
      bot: build(:bot),
      geofence_event: :enter
    }
    |> merge_attributes(attrs)
    |> Notification.pack_virtual_fields()
  end

  def user_invitation_notification_factory do
    %Notification{
      type: :user_invitation,
      user: build(:user),
      other_user: build(:user),
      share_type: :always
    }
  end

  def bot_invitation_event_factory do
    %Wocky.Events.BotInvitation{
      to: build(:user),
      from: build(:user),
      bot: build(:bot),
      invitation: build(:bot_invitation)
    }
  end

  def bot_invitation_response_event_factory do
    %Wocky.Events.BotInvitationResponse{
      to: build(:user),
      from: build(:user),
      bot: build(:bot),
      invitation: build(:bot_invitation)
    }
  end

  def bot_item_event_factory do
    %Wocky.Events.BotItem{
      to: build(:user),
      from: build(:user),
      item: build(:item)
    }
  end

  def geofence_event_factory do
    %Wocky.Events.GeofenceEvent{
      to: build(:user),
      from: build(:user),
      bot: build(:bot),
      event: :enter
    }
  end

  def location_request_event_factory do
    %Wocky.Events.LocationRequest{
      to: build(:user)
    }
  end

  def location_share_event_factory do
    %Wocky.Events.LocationShare{
      to: build(:user),
      from: build(:user),
      expires_at: DateTime.utc_now(),
      share_id: 1,
      share_type: :always,
      other_user_share_type: :always,
      new_friend?: false
    }
  end

  def new_message_event_factory do
    %Wocky.Events.NewMessage{
      to: build(:user),
      from: build(:user),
      content: "testing"
    }
  end

  def user_invitation_event_factory do
    %Wocky.Events.UserInvitation{
      to: build(:user),
      from: build(:user)
    }
  end

  def user_invitation_response_event_factory do
    %Wocky.Events.UserInvitationResponse{
      to: build(:user),
      from: build(:user)
    }
  end

  def user_invite_code_factory do
    %InviteCode{
      code: InviteCode.generate(),
      user: build(:user),
      phone_number: phone_number(),
      share_type: :always
    }
  end

  def metadata_factory do
    %Wocky.Server.Metadata{
      key: Lorem.word(),
      value: Lorem.sentence(),
      description: Lorem.sentence()
    }
  end

  def phone_number do
    "+1555#{Phone.exchange_code()}#{Phone.extension()}"
  end

  def external_id do
    Base.encode16(:crypto.strong_rand_bytes(10))
  end

  def device do
    String.base64()
  end

  # Handles have a more restricted set of characters than any of the Faker
  # functions provide, so we provide our own function for constructing them
  def handle do
    Base.encode32(:crypto.strong_rand_bytes(10))
  end
end
