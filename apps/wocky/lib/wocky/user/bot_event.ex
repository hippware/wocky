defmodule Wocky.User.BotEvent do
  @moduledoc """
  Represents a user event.
  """

  use Wocky.Repo.Schema

  import Ecto.Query
  import EctoHomoiconicEnum, only: [defenum: 2]

  alias Wocky.Bot
  alias Wocky.Repo
  alias Wocky.User
  alias Wocky.User.Location

  defenum EventType, [
    :enter,
    :exit,
    :transition_in,
    :transition_out,
    :timeout,
    :deactivate,
    :reactivate
  ]

  @foreign_key_type :binary_id
  @primary_key {:id, :binary_id, autogenerate: true}
  schema "user_bot_events" do
    field :device, :string
    field :event, EventType, null: false
    field :occurred_at, :utc_datetime, null: false

    timestamps(updated_at: false)

    belongs_to :user, User
    belongs_to :bot, Bot
    belongs_to :location, Location
  end

  @type event ::
          :enter
          | :exit
          | :transition_in
          | :transition_out
          | :timeout
          | :reactivate
          | :deactivate

  @type t :: %BotEvent{
          id: binary,
          user_id: User.id(),
          device: User.device(),
          bot_id: Bot.id(),
          location_id: binary,
          event: event,
          occurred_at: DateTime.t()
        }

  @insert_fields [
    :user_id,
    :device,
    :bot_id,
    :location_id,
    :event,
    :occurred_at
  ]

  @spec get_last_events(User.id()) :: [t]
  def get_last_events(user_id) do
    BotEvent
    |> distinct(:bot_id)
    |> where(user_id: ^user_id)
    |> order_by(desc: :created_at)
    |> Repo.all()
  end

  @spec get_last_event(User.id(), Bot.id()) :: t | nil
  def get_last_event(user_id, bot_id) do
    user_id
    |> get_last_event_query(bot_id)
    |> Repo.one()
  end

  @spec get_last_event_type(User.id(), Bot.id()) :: event | nil
  def get_last_event_type(user_id, bot_id) do
    user_id
    |> get_last_event_query(bot_id)
    |> select([e], e.event)
    |> Repo.one()
  end

  defp get_last_event_query(user_id, bot_id) do
    BotEvent
    |> where(user_id: ^user_id, bot_id: ^bot_id)
    |> order_by(desc: :created_at)
    |> limit(1)
  end

  @doc """
  Insert a system-generated event; i.e., one that is not tied to the user
  changing location.
  """
  @spec insert_system(User.t(), Bot.t(), event, String.t()) :: t
  def insert_system(user, bot, event, reason),
    do: insert(user, "System/#{reason}", bot, nil, event)

  @spec insert(User.t(), User.device(), Bot.t(), Location.t() | nil, event) :: t
  def insert(user, device, bot, loc \\ nil, event) do
    %{
      user_id: user.id,
      device: device,
      bot_id: bot.id,
      location_id: loc && loc.id,
      event: event,
      occurred_at: (loc && loc.captured_at) || DateTime.utc_now()
    }
    |> changeset()
    |> Repo.insert!()
  end

  defp changeset(params) do
    %BotEvent{}
    |> cast(params, @insert_fields)
    |> validate_required([:user_id, :device, :bot_id, :event])
    |> foreign_key_constraint(:user_id)
    |> foreign_key_constraint(:bot_id)
    |> foreign_key_constraint(:location_id)
  end
end
