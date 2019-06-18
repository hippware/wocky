defmodule Wocky.Factory do
  @moduledoc """
  Factory for non-Ecto structures
  """

  use ExMachina

  alias Faker.Internet
  alias Faker.Lorem
  alias Wocky.Audit.PushLog
  alias Wocky.Audit.TrafficLog
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
end
