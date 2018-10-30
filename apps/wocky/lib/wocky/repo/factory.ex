defmodule Wocky.Repo.Factory do
  @moduledoc false

  use ExMachina.Ecto, repo: Wocky.Repo
  use Wocky.JID

  alias Faker.Address
  alias Faker.Company
  alias Faker.Internet
  alias Faker.Lorem
  alias Faker.Name
  alias Faker.Phone.EnUs, as: Phone
  alias Faker.String
  alias Wocky.Bot
  alias Wocky.Bot.Invitation
  alias Wocky.Bot.Item
  alias Wocky.Bot.Subscription
  alias Wocky.GeoUtils
  alias Wocky.Message
  alias Wocky.Push.Log, as: PushLog
  alias Wocky.Push.Token, as: PushToken
  alias Wocky.Repo.ID
  alias Wocky.Roster.InitialContact
  alias Wocky.Roster.Item, as: RosterItem
  alias Wocky.TrafficLog
  alias Wocky.TROS
  alias Wocky.TROS.Metadata, as: TROSMetadata
  alias Wocky.User
  alias Wocky.User.Location
  alias Wocky.User.Notification

  def user_factory do
    user_id = ID.new()

    %User{
      id: user_id,
      username: user_id,
      external_id: external_id(),
      handle: new_handle(),
      avatar: TROS.make_url(ID.new()),
      first_name: Name.first_name(),
      last_name: Name.last_name(),
      phone_number: phone_number(),
      email: Internet.email(),
      tagline: Lorem.sentence(),
      provider: "local",
      roles: [],
      welcome_sent: false
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
      image: TROS.make_url(ID.new()),
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
      stanza: "<text>" <> Lorem.sentence() <> "</text>"
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
      message: Lorem.paragraph()
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
      name: Name.first_name(),
      ask: :none,
      subscription: :both,
      groups: []
    }
  end

  def traffic_log_factory do
    %TrafficLog{
      resource: Lorem.word(),
      ip: Internet.ip_v6_address() <> ":5020",
      host: Internet.domain_name(),
      packet: Lorem.paragraph(),
      incoming: false
    }
  end

  def location_factory do
    %Location{
      resource: String.base64(),
      lat: Address.latitude(),
      lon: Address.longitude(),
      accuracy: 10,
      is_fetch: false
    }
  end

  def push_log_factory do
    %PushLog{
      user: build(:user),
      resource: Lorem.word(),
      token: ID.new(),
      message_id: ID.new(),
      payload: ~s(%{"aps" => %{"alert" => #{Lorem.sentence()}}}),
      response: "success"
    }
  end

  def initial_contact_factory do
    %InitialContact{
      type: Enum.random(["follower", "followee", "friend"])
    }
  end

  def push_token_factory do
    %PushToken{
      user: build(:user),
      resource: ID.new(),
      token: ID.new(),
      valid: true,
      enabled_at: DateTime.utc_now()
    }
  end

  def invitation_factory do
    %Invitation{
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
      invitation: nil,
      geofence_event: nil,
      invitation_accepted: nil
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

  def invitation_notification_factory do
    inviter = build(:user)
    bot = build(:bot, user: inviter)

    %Notification{
      type: :invitation,
      user: build(:user),
      other_user: inviter,
      bot: bot
    }
  end

  def invitation_response_notification_factory do
    %{
      invitation_notification_factory()
      | type: :invitation_response,
        invitation_accepted: true
    }
  end

  def user_follow_notification_factory do
    %Notification{
      type: :user_follow,
      user: build(:user),
      other_user: build(:user)
    }
  end

  def phone_number do
    "+1555#{Phone.area_code()}#{Phone.extension()}"
  end

  def external_id do
    Base.encode16(:crypto.strong_rand_bytes(10))
  end

  def new_jid do
    ID.new() |> JID.make(Lorem.word(), Lorem.word()) |> JID.to_binary()
  end

  # Handles have a more restricted set of characters than any of the Faker
  # functions provide, so we provide our own function for constructing them
  def new_handle do
    Base.encode32(:crypto.strong_rand_bytes(10))
  end

  def make_login do
    user = insert(:user)
    {:ok, {token, _}} = Wocky.Account.assign_token(user.id, "testing")
    {:ok, user.id, token}
  end

  def image_url(image), do: TROS.make_url(image.id)
end
