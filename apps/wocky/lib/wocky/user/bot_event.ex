defmodule Wocky.User.BotEvent do
  @moduledoc """
  Represents a user event.
  """

  use Wocky.Repo.Model

  alias Wocky.Bot
  alias Wocky.User
  alias __MODULE__, as: BotEvent

  @foreign_key_type :binary_id
  @primary_key {:id, :binary_id, autogenerate: true}
  schema "user_bot_events" do
    field :event,  :string, null: false

    timestamps()

    belongs_to :user, User
    belongs_to :bot, Bot
  end

  @type event :: :enter | :exit
  @type t :: %BotEvent{
    id: binary,
    user_id: User.id,
    bot_id: Bot.id,
    event: event
  }

  @spec get_last_event_type(User.id, Bot.id) :: :enter | :exit | nil
  def get_last_event_type(user_id, bot_id) do
    event = Repo.one(
      from e in BotEvent,
        where: e.user_id == ^user_id and e.bot_id == ^bot_id,
        select: e.event,
        order_by: [desc: :created_at],
        limit: 1
    )

    case event do
      nil -> nil
      "enter" -> :enter
      "exit" -> :exit
    end
  end

  @spec add_event(User.id, Bot.id, event) :: :ok
  def add_event(user_id, bot_id, event) do
    Repo.insert!(
      %BotEvent{
        user_id: user_id,
        bot_id: bot_id,
        event: to_string(event)
      }
    )
  end
end
