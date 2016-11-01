defmodule Wocky.Factory do
  @moduledoc false

  use ExMachina
  use Wocky.InsertStrategy
  alias Wocky.Bot
  alias Wocky.User
  alias Wocky.Location

  def phone_number do
    "+1555#{Faker.Phone.EnUs.area_code}#{Faker.Phone.EnUs.extension}"
  end

  def user_factory do
    %User{
      user: User.make_id,
      server: :wocky_app.server,
      handle: Faker.Internet.user_name,
      password: "password",
      avatar: :tros.make_url(:wocky_app.server, :wocky_db.create_id),
      first_name: Faker.Name.first_name,
      last_name: Faker.Name.last_name,
      email: Faker.Internet.email,
      phone_number: phone_number,
      external_id: Faker.Code.isbn13
    }
  end

  def bot_factory do
    %Bot{
      id: Bot.make_id,
      server: :wocky_app.server,
      title: Faker.Company.name,
      shortname: Faker.Company.buzzword,
      owner: User.make_id,
      description: Faker.Lorem.paragraph(%Range{first: 1, last: 2}),
      image: :tros.make_url(:wocky_app.server, :wocky_db.create_id),
      type: "test",
      address: Faker.Address.street_address,
      lat: Faker.Address.latitude,
      lon: Faker.Address.longitude,
      radius: :rand.uniform(100),
      visibility: 1,
      alerts: 1,
      updated: :wocky_db.now_to_timestamp(:erlang.timestamp)
    }
  end

  def with_bots(user, num) do
    [user | build_list(num, :bot, %{owner: user[:user]})]
  end

  def location_factory do
    %Location{
      lat: Faker.Address.latitude,
      lon: Faker.Address.longitude,
      accuracy: 10
    }
  end
end
