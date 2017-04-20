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
  alias Wocky.Conversation
  alias Wocky.Repo.ID
  alias Wocky.TROS
  alias Wocky.TROS.Metadata, as: TROSMetadata
  alias Wocky.User
  alias Wocky.User.Location

  def user_factory do
    user_id = ID.new
    %User{
      id: user_id,
      username: user_id,
      server: "localhost",
      external_id: Code.isbn13,
      handle: Internet.user_name,
      avatar: TROS.make_url("localhost", ID.new),
      first_name: Name.first_name,
      last_name: Name.last_name,
      phone_number: phone_number(),
      email: Internet.email
    }
  end

  def conversation_factory do
    message = "<message>" <> Lorem.sentence() <> "</message>"
    %Conversation{
      other_jid: new_jid(),
      message: message,
      outgoing: true
    }
  end

  def tros_metadata_factory do
    %TROSMetadata{
      id: ID.new,
      access: Lorem.sentence
    }
  end

  def bot_factory do
    %Bot{
      id: ID.new,
      server: "localhost",
      title: Company.name,
      shortname: Company.buzzword,
      user_id: ID.new,
      description: Lorem.paragraph(%Range{first: 1, last: 2}),
      image: TROS.make_url("localhost", ID.new),
      type: "test",
      address: Address.street_address,
      lat: Address.latitude,
      lon: Address.longitude,
      radius: :rand.uniform(100) * 1000,
      visibility: 100,
      alerts: true,
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

  defp phone_number do
    "+1555#{Phone.area_code}#{Phone.extension}"
  end

  defp new_jid do
    ID.new |> JID.make(Lorem.word, Lorem.word) |> JID.to_binary
  end
end
