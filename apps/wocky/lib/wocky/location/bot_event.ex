defmodule Wocky.Location.BotEvent do
  @moduledoc """
  Represents a user event.
  """

  use Wocky.Repo.Schema

  import Ecto.Query
  import EctoEnum

  alias Wocky.Account.User
  alias Wocky.Location.UserLocation
  alias Wocky.POI.Bot
  alias Wocky.Repo

  defenum(EventTypeEnum, :event_type, [
    :enter,
    :exit,
    :transition_in,
    :transition_out,
    :timeout,
    :deactivate,
    :reactivate
  ])

  @foreign_key_type :binary_id
  @primary_key {:id, :binary_id, autogenerate: true}
  schema "user_bot_events" do
    field :device, :string
    field :event, EventTypeEnum, null: false
    field :occurred_at, :utc_datetime_usec, null: false
    field :location_id, :string

    timestamps(updated_at: false)

    belongs_to :user, User
    belongs_to :bot, Bot
  end

  @type event ::
          :enter
          | :exit
          | :transition_in
          | :transition_out
          | :timeout
          | :reactivate
          | :deactivate

  @type t :: %__MODULE__{
          id: String.t(),
          user_id: User.id(),
          device: User.device(),
          bot_id: Bot.id(),
          event: event(),
          occurred_at: DateTime.t(),
          location_id: String.t()
        }

  @type bot_event_map() :: %{required(Bot.id()) => t()}

  @insert_fields [
    :user_id,
    :device,
    :bot_id,
    :event,
    :occurred_at,
    :location_id
  ]

  @spec get_last_events(User.tid()) :: bot_event_map()
  def get_last_events(user) do
    __MODULE__
    |> distinct(:bot_id)
    |> where(user_id: ^User.id(user))
    |> order_by(desc: :created_at)
    |> Repo.all()
    |> Enum.map(fn e -> {e.bot_id, e} end)
    |> Enum.into(%{})
  end

  @spec get_last_event(User.tid(), Bot.id()) :: t() | nil
  def get_last_event(user, bot_id) do
    user
    |> get_last_event_query(bot_id)
    |> Repo.one()
  end

  @spec get_last_event_type(User.tid(), Bot.id()) :: event() | nil
  def get_last_event_type(user, bot_id) do
    user
    |> get_last_event_query(bot_id)
    |> select([e], e.event)
    |> Repo.one()
  end

  defp get_last_event_query(user, bot_id) do
    __MODULE__
    |> where(user_id: ^User.id(user), bot_id: ^bot_id)
    |> order_by(desc: :created_at)
    |> limit(1)
  end

  @spec new(User.tid(), User.device(), Bot.t(), UserLocation.t() | nil, event) ::
          map()
  def new(user, device, bot, loc \\ nil, event) do
    %{
      user_id: User.id(user),
      device: device,
      bot_id: bot.id,
      event: event,
      created_at: DateTime.utc_now(),
      occurred_at: (loc && loc.captured_at) || DateTime.utc_now(),
      location_id: loc && loc.id
    }
  end

  @doc """
  Insert a system-generated event; i.e., one that is not tied to the user
  changing location.
  """
  @spec insert_system(User.tid(), Bot.t(), event, String.t()) ::
          Repo.result(t())
  def insert_system(user, bot, event, reason),
    do: insert(user, "System/#{reason}", bot, nil, event)

  @spec insert(
          User.tid(),
          User.device(),
          Bot.t(),
          UserLocation.t() | nil,
          event
        ) :: Repo.result(t())
  def insert(user, device, bot, loc \\ nil, event) do
    user
    |> new(device, bot, loc, event)
    |> changeset()
    |> Repo.insert()
  end

  defp changeset(params) do
    %__MODULE__{}
    |> cast(params, @insert_fields)
    |> validate_required([:user_id, :device, :bot_id, :event])
    |> foreign_key_constraint(:user_id)
    |> foreign_key_constraint(:bot_id)
  end
end
