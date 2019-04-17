defmodule Wocky.Repo.Factory do
  @moduledoc false

  use ExMachina.Ecto, repo: Wocky.Repo

  alias Faker.Address
  alias Faker.Company
  alias Faker.Internet
  alias Faker.Lorem
  alias Faker.Name
  alias Faker.Phone.EnUs, as: Phone
  alias Faker.String
  alias Wocky.Account.JWT.Client, as: ClientJWT
  alias Wocky.Account.JWT.Server, as: ServerJWT
  alias Wocky.Bot
  alias Wocky.Bot.Invitation, as: BotInvitation
  alias Wocky.Bot.Item
  alias Wocky.Bot.Subscription
  alias Wocky.GeoUtils
  alias Wocky.Message
  alias Wocky.Notifier.InBand.Notification
  alias Wocky.Notifier.Push.Log, as: PushLog
  alias Wocky.Notifier.Push.Token, as: PushToken
  alias Wocky.Repo.ID
  alias Wocky.Repo.Timestamp
  alias Wocky.Roster.Invitation, as: RosterInvitation
  alias Wocky.Roster.Item, as: RosterItem
  alias Wocky.TrafficLog
  alias Wocky.TROS
  alias Wocky.TROS.Metadata, as: TROSMetadata
  alias Wocky.User
  alias Wocky.User.Location
  alias Wocky.User.LocationShare

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
      content: Lorem.paragraph()
    }
  end

  def tros_metadata_factory do
    %TROSMetadata{
      id: ID.new(),
      user: build(:user),
      access: Lorem.sentence(),
      ready: true
    }
  end

  def roster_item_factory do
    %RosterItem{
      name: Name.first_name()
    }
  end

  def user_invitation_factory do
    %RosterInvitation{
      user: build(:user),
      invitee: build(:user)
    }
  end

  def user_location_share_factory do
    %LocationShare{
      user: build(:user),
      shared_with: build(:user),
      expires_at: Timestamp.shift(days: 1)
    }
  end

  def traffic_log_factory do
    %TrafficLog{
      device: device(),
      ip: Internet.ip_v6_address() <> ":5020",
      host: Internet.domain_name(),
      packet: Lorem.paragraph(),
      incoming: false
    }
  end

  def location_factory do
    %Location{
      device: device(),
      lat: Address.latitude(),
      lon: Address.longitude(),
      accuracy: 10
    }
  end

  def push_log_factory do
    %PushLog{
      user: build(:user),
      device: device(),
      token: ID.new(),
      message_id: ID.new(),
      payload: ~s(%{"aps" => %{"alert" => #{Lorem.sentence()}}}),
      response: "success"
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

  def notification_factory do
    %Notification{
      user: build(:user),
      other_user: build(:user),
      bot: nil,
      bot_item: nil,
      bot_invitation: nil,
      geofence_event: nil,
      bot_invitation_accepted: nil
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

  def bot_invitation_response_notification_factory do
    %{
      bot_invitation_notification_factory()
      | type: :bot_invitation_response,
        bot_invitation_accepted: true
    }
  end

  def bot_item_notification_factory do
    owner = build(:user)
    bot = build(:bot, user: owner)

    %Notification{
      type: :bot_item,
      user: owner,
      other_user: build(:user),
      bot: bot,
      bot_item: build(:item)
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

  def location_share_notification_factory do
    %Notification{
      type: :location_share,
      user: build(:user),
      other_user: build(:user),
      expires_at: Timestamp.shift(days: 1)
    }
  end

  def location_share_end_notification_factory do
    %Notification{
      type: :location_share_end,
      user: build(:user),
      other_user: build(:user)
    }
  end

  def user_invitation_notification_factory do
    %Notification{
      type: :user_invitation,
      user: build(:user),
      other_user: build(:user)
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

  def image_url(image), do: TROS.make_url(image.id)

  def get_test_token(user) do
    {:ok, token, _} = ClientJWT.encode_and_sign(user)
    token
  end

  def get_test_location_token(user) do
    {:ok, token, _} = ServerJWT.encode_and_sign(user)
    token
  end
end
