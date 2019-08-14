defmodule Wocky.Factory do
  @moduledoc """
  Factory for non-Ecto structures
  """

  use ExMachina

  alias Ecto.UUID
  alias Faker.Address
  alias Faker.Internet
  alias Faker.Lorem
  alias Wocky.Audit.PushLog
  alias Wocky.Audit.TrafficLog
  alias Wocky.Location.UserLocation
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID

  def traffic_log_factory do
    %TrafficLog{
      user_id: Factory.build(:user),
      device: Factory.device(),
      ip: Internet.ip_v6_address() <> ":5020",
      host: Internet.domain_name(),
      packet: Lorem.paragraph(),
      incoming: false
    }
  end

  def push_log_factory do
    %PushLog{
      user_id: ID.new(),
      device: Factory.device(),
      token: ID.new(),
      message_id: ID.new(),
      payload: ~s(%{"aps" => %{"alert" => #{Lorem.sentence()}}}),
      response: "success"
    }
  end

  def user_location_factory do
    %UserLocation{
      user_id: Factory.build(:user).id,
      device: Factory.device(),
      lat: Address.latitude(),
      lon: Address.longitude(),
      accuracy: 0.0,
      speed: 0.0,
      heading: 0.0,
      altitude: 0.0,
      altitude_accuracy: 0.0,
      captured_at: DateTime.utc_now(),
      uuid: UUID.generate(),
      is_moving: false,
      odometer: 0.0,
      activity: "",
      activity_confidence: 0,
      battery_level: 100.0,
      battery_charging: false,
      created_at: DateTime.utc_now()
    }
  end
end
