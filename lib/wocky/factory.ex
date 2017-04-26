defmodule Wocky.Factory do
  @moduledoc false

  use ExMachina
  use Wocky.InsertStrategy
  use Wocky.JID
  use Exref, ignore: [
    build: 1, build: 2, build_list: 2, build_list: 3, build_pair: 1,
    build_pair: 2, create: 1, create: 2, create_pair: 2, create_list: 3,
    factory: 1, insert: 1, insert: 2, insert_list: 2, insert_list: 3,
    insert_pair: 1, insert_pair: 2, bot_factory: 0, location_factory: 0,
    user_factory: 0
  ]

  alias Faker.Address
  alias Faker.Code
  alias Faker.Company
  alias Faker.Internet
  alias Faker.Lorem
  alias Faker.Name
  alias Faker.Phone.EnUs, as: Phone
  alias Wocky.Bot
  alias Wocky.User
  alias Wocky.Location

  defp phone_number do
    "+1555#{Phone.area_code}#{Phone.extension}"
  end

  def user_factory do
    %User{
      user: User.make_id,
      server: :wocky_app.server,
      handle: Internet.user_name,
      password: "password",
      avatar: :tros.make_url(:wocky_app.server, :wocky_db.create_id),
      first_name: Name.first_name,
      last_name: Name.last_name,
      email: Internet.email,
      phone_number: phone_number(),
      external_id: Code.isbn13
    }
  end

  def bot_factory do
    %Bot{
      id: Bot.make_id,
      server: :wocky_app.server,
      title: Company.name,
      shortname: Company.buzzword,
      owner: JID.to_binary(JID.make!(User.make_id, :wocky_app.server, <<>>)),
      description: Lorem.paragraph(%Range{first: 1, last: 2}),
      image: :tros.make_url(:wocky_app.server, :wocky_db.create_id),
      type: "test",
      address: Address.street_address,
      lat: Address.latitude,
      lon: Address.longitude,
      radius: :rand.uniform(100) * 1000,
      visibility: 1,
      alerts: 1,
      updated: :wocky_db.now_to_timestamp(:erlang.timestamp),
      follow_me: false
    }
  end

  def location_factory do
    %Location{
      lat: Address.latitude,
      lon: Address.longitude,
      accuracy: 10
    }
  end
end
