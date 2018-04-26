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
    field :event, EventType, null: false

    timestamps()

    belongs_to :user, User
    belongs_to :bot, Bot
  end

  @type event :: :enter | :exit | :transition_in | :transition_out
  @type t :: %BotEvent{
          id: binary,
          user_id: User.id(),
          bot_id: Bot.id(),
          event: event
        }

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
    from e in BotEvent,
      where: e.user_id == ^user_id and e.bot_id == ^bot_id,
      order_by: [desc: :created_at],
      limit: 1
  end

  @spec insert(User.t(), Bot.t(), event) :: t
  def insert(user, bot, event) do
    %BotEvent{user: user, bot: bot}
    |> changeset(%{event: event})
    |> Repo.insert!()
  end

  defp changeset(struct, params) do
    struct
    |> cast(params, [:event])
    |> validate_required([:event])
    |> foreign_key_constraint(:user_id)
    |> foreign_key_constraint(:bot_id)
  end
end
