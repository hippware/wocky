defmodule Wocky.Factory do
  @moduledoc false

  use ExMachina
  use Wocky.InsertStrategy
  use Wocky.Ejabberd

  alias Faker.Address
  alias Faker.Code
  alias Faker.Company
  alias Faker.Internet
  alias Faker.Lorem
  alias Faker.Name
  alias Faker.Phone.EnUs, as: Phone
  alias Wocky.Bot
  alias Wocky.ID
  alias Wocky.Location
  alias Wocky.User

  defp phone_number do
    "+1555#{Phone.area_code}#{Phone.extension}"
  end

  def user_factory do
    %User{
      id: ID.new,
      server: :wocky_app.server,
      handle: Internet.user_name,
      # avatar: :tros.make_url(:wocky_app.server, ID.new),
      first_name: Name.first_name,
      last_name: Name.last_name,
      email: Internet.email,
      phone_number: phone_number(),
      external_id: Code.isbn13
    }
  end

  def bot_factory do
    %Bot{
      id: ID.new,
      server: :wocky_app.server,
      title: Company.name,
      shortname: Company.buzzword,
      owner: :jid.to_binary(:jid.make(ID.new, :wocky_app.server, <<>>)),
      description: Lorem.paragraph(%Range{first: 1, last: 2}),
      image: :tros.make_url(:wocky_app.server, ID.new),
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
