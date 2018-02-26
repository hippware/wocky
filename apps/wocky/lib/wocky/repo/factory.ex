defmodule Wocky.Repo.Factory do
  @moduledoc false

  use ExMachina.Ecto, repo: Wocky.Repo
  use Wocky.JID

  alias Faker.Address
  alias Faker.Code
  alias Faker.Company
  alias Faker.Internet
  alias Faker.Lorem
  alias Faker.Name
  alias Faker.Phone.EnUs, as: Phone
  alias Wocky.Bot
  alias Wocky.Bot.Item
  alias Wocky.Bot.Share
  alias Wocky.Bot.Subscription
  alias Wocky.Conversation
  alias Wocky.GeoUtils
  alias Wocky.InitialContact
  alias Wocky.HomeStream.Item, as: HomeStreamItem
  alias Wocky.Push.Log, as: PushLog
  alias Wocky.Push.Token, as: PushToken
  alias Wocky.Repo.ID
  alias Wocky.RosterItem
  alias Wocky.TrafficLog
  alias Wocky.TROS
  alias Wocky.TROS.Metadata, as: TROSMetadata
  alias Wocky.User
  alias Wocky.User.Location

  def user_factory do
    user_id = ID.new()

    %User{
      id: user_id,
      username: user_id,
      server: "localhost",
      external_id: Code.isbn13(),
      handle: new_handle(),
      avatar: TROS.make_url("localhost", ID.new()),
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
      server: "localhost",
      user: build(:user),
      pending: false,
      title: Company.name(),
      shortname: Lorem.sentence(),
      description: Lorem.paragraph(%Range{first: 1, last: 2}),
      image: TROS.make_url("localhost", ID.new()),
      type: "test",
      address: Address.street_address(),
      address_data: "{name: foo}",
      location: GeoUtils.point(Address.latitude(), Address.longitude()),
      radius: :rand.uniform(100),
      public: false,
      alerts: false,
      follow_me: false,
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

  def share_factory do
    %Share{
      user: build(:user),
      bot: build(:bot),
      sharer: build(:user)
    }
  end

  def subscription_factory do
    %Subscription{
      user: build(:user),
      bot: build(:bot)
    }
  end

  def conversation_factory do
    message = "<message>" <> Lorem.sentence() <> "</message>"

    %Conversation{
      id: :rand.uniform(0x7FFFFFFFFFFFFFFF),
      other_jid: new_jid(),
      message: message,
      outgoing: true
    }
  end

  def tros_metadata_factory do
    %TROSMetadata{
      id: ID.new(),
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
      lat: Address.latitude(),
      lon: Address.longitude(),
      accuracy: 10
    }
  end

  def home_stream_item_factory do
    %HomeStreamItem{
      key: new_jid(),
      from_jid: new_jid(),
      stanza: Lorem.paragraph(),
      class: :item
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

  def phone_number do
    "+1555#{Phone.area_code()}#{Phone.extension()}"
  end

  def new_jid do
    ID.new() |> JID.make(Lorem.word(), Lorem.word()) |> JID.to_binary()
  end

  # Handles have a more restricted set of characters than any of the Faker
  # functions provide, so we provide our own function for constructing them
  def new_handle do
    Base.encode32(:crypto.strong_rand_bytes(10))
  end
end
