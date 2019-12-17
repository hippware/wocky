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
  alias Wocky.Account.User
  alias Wocky.Contacts.Relationship
  alias Wocky.Events.BotInvitation, as: BotInvitationEvent
  alias Wocky.Events.BotInvitationResponse
  alias Wocky.Events.BotItem
  alias Wocky.Events.GeofenceEvent
  alias Wocky.Events.LocationRequest
  alias Wocky.Events.LocationShare
  alias Wocky.Events.NewMessage
  alias Wocky.Events.UserInvitation
  alias Wocky.Events.UserInvitationResponse
  alias Wocky.GeoUtils
  alias Wocky.Location.BotEvent
  alias Wocky.Location.UserLocation
  alias Wocky.Location.UserProximity.Subscription, as: ProxSubscription
  alias Wocky.Messaging.Message
  alias Wocky.Notifier.InBand.Notification
  alias Wocky.Notifier.Push.Token, as: PushToken
  alias Wocky.POI.Bot
  alias Wocky.POI.Item
  alias Wocky.Relation.Invitation, as: BotInvitation
  alias Wocky.Relation.Subscription
  alias Wocky.Repo.ID
  alias Wocky.TROS
  alias Wocky.TROS.Metadata, as: TROSMetadata

  def user_factory do
    %User{
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
    %Bot{
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
    %Item{
      id: ID.new(),
      bot: build(:bot),
      content: Lorem.sentence()
    }
  end

  def subscription_factory do
    %Subscription{
      user: build(:user),
      bot: build(:bot)
    }
  end

  def message_factory do
    %Message{
      sender: build(:user),
      recipient: build(:user),
      content: Lorem.paragraph(),
      client_data: Lorem.paragraph(),
      read: false
    }
  end

  def tros_metadata_factory do
    %TROSMetadata{
      id: ID.new(),
      user: build(:user),
      access: Lorem.sentence(),
      ready: true,
      available_formats: [:full, :original, :thumbnail]
    }
  end

  def friend_factory do
    %Relationship{
      user: build(:user),
      contact: build(:user),
      share_type: :always,
      state: :friend
    }
  end

  def location_factory do
    %UserLocation{
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
    %BotEvent{
      bot: build(:bot),
      user: build(:user),
      device: device(),
      event: :enter,
      occurred_at: DateTime.utc_now()
    }
  end

  def push_token_factory do
    %PushToken{
      user: build(:user),
      device: device(),
      token: ID.new(),
      valid: true,
      enabled_at: DateTime.utc_now()
    }
  end

  def bot_invitation_factory do
    %BotInvitation{
      user: build(:user),
      invitee: build(:user),
      bot: build(:bot),
      accepted: nil
    }
  end

  def bot_invitation_notification_factory do
    inviter = build(:user)
    bot = build(:bot, user: inviter)

    %Notification{
      type: :bot_invitation,
      user: build(:user),
      other_user: inviter,
      bot: bot
    }
  end

  def geofence_event_notification_factory do
    %Notification{
      type: :geofence_event,
      user: build(:user),
      other_user: build(:user),
      bot: build(:bot),
      geofence_event: :enter
    }
  end

  def bot_invitation_event_factory do
    %BotInvitationEvent{
      to: build(:user),
      from: build(:user),
      bot: build(:bot),
      invitation: build(:bot_invitation)
    }
  end

  def bot_invitation_response_event_factory do
    %BotInvitationResponse{
      to: build(:user),
      from: build(:user),
      bot: build(:bot),
      invitation: build(:bot_invitation)
    }
  end

  def bot_item_event_factory do
    %BotItem{
      to: build(:user),
      from: build(:user),
      item: build(:item)
    }
  end

  def geofence_event_factory do
    %GeofenceEvent{
      to: build(:user),
      from: build(:user),
      bot: build(:bot),
      event: :enter
    }
  end

  def location_request_event_factory do
    %LocationRequest{
      to: build(:user)
    }
  end

  def location_share_event_factory do
    %LocationShare{
      to: build(:user),
      from: build(:user),
      expires_at: DateTime.utc_now()
    }
  end

  def new_message_event_factory do
    %NewMessage{
      to: build(:user),
      from: build(:user),
      content: "testing"
    }
  end

  def user_invitation_event_factory do
    %UserInvitation{
      to: build(:user),
      from: build(:user)
    }
  end

  def user_invitation_response_event_factory do
    %UserInvitationResponse{
      to: build(:user),
      from: build(:user)
    }
  end

  def user_proximity_factory do
    %ProxSubscription{
      user: build(:user),
      target: build(:user),
      range: 10,
      cooldown: :timer.hours(24),
      last_notification: nil
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
