defmodule Wocky.Factory do
  @moduledoc false

  use ExMachina
  use Wocky.InsertStrategy
  use Wocky.Ejabberd
  use Exref, ignore: [
    build: 1, build: 2, build_list: 2, build_list: 3, build_pair: 1,
    build_pair: 2, create: 1, create: 2, create_pair: 2, create_list: 3,
    factory: 1, insert: 1, insert: 2, insert_list: 2, insert_list: 3,
    insert_pair: 1, insert_pair: 2, bot_factory: 0, location_factory: 0,
    user_factory: 0
  ]
  alias Wocky.ID
  alias Wocky.Bot
  alias Wocky.User
  alias Wocky.Location

  defp phone_number do
    "+1555#{Faker.Phone.EnUs.area_code}#{Faker.Phone.EnUs.extension}"
  end

  def user_factory do
    %User{
      user: ID.new,
      server: :wocky_app.server,
      handle: Faker.Internet.user_name,
      password: "password",
      avatar: :tros.make_url(:wocky_app.server, ID.new),
      first_name: Faker.Name.first_name,
      last_name: Faker.Name.last_name,
      email: Faker.Internet.email,
      phone_number: phone_number(),
      external_id: Faker.Code.isbn13
    }
  end

  def bot_factory do
    %Bot{
      id: ID.new,
      server: :wocky_app.server,
      title: Faker.Company.name,
      shortname: Faker.Company.buzzword,
      owner: :jid.to_binary(:jid.make(ID.new, :wocky_app.server, <<>>)),
      description: Faker.Lorem.paragraph(%Range{first: 1, last: 2}),
      image: :tros.make_url(:wocky_app.server, ID.new),
      type: "test",
      address: Faker.Address.street_address,
      lat: Faker.Address.latitude,
      lon: Faker.Address.longitude,
      radius: :rand.uniform(100) * 1000,
      visibility: 1,
      alerts: 1,
      updated: :wocky_db.now_to_timestamp(:erlang.timestamp),
      follow_me: false
    }
  end

  def location_factory do
    %Location{
      lat: Faker.Address.latitude,
      lon: Faker.Address.longitude,
      accuracy: 10
    }
  end
end
