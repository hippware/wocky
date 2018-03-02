defmodule Wocky.User.Location do
  @moduledoc "Interface for user location processing."

  use Wocky.JID
  use Wocky.Repo.Schema

  alias Wocky.Bot
  alias Wocky.GeoUtils
  alias Wocky.Push
  alias Wocky.Push.Events.BotPerimeterEvent
  alias Wocky.Repo
  alias Wocky.User
  alias Wocky.User.BotEvent
  alias __MODULE__

  require Logger

  @foreign_key_type :binary_id
  @primary_key {:id, :binary_id, autogenerate: true}
  schema "user_locations" do
    field :user_id, :binary_id, null: false
    field :resource, :string, null: false
    field :lat, :float, null: false
    field :lon, :float, null: false
    field :accuracy, :float

    timestamps()

    belongs_to :user, User, define_field: false
  end

  @type location_tuple :: {float, float, float}
  @type t :: %Location{
          user_id: User.id(),
          resource: User.resource(),
          lat: float,
          lon: float,
          accuracy: float
        }

  @doc "Store a user location datapoint"
  @spec insert(User.t(), User.resource(), float, float, float) ::
          {:ok, t} | {:error, any}
  def insert(user, resource, lat, lon, accuracy) do
    {nlat, nlon} = GeoUtils.normalize_lat_lon(lat, lon)
    data = %{resource: resource, lat: nlat, lon: nlon, accuracy: accuracy}

    user
    |> Ecto.build_assoc(:locations)
    |> changeset(data)
    |> Repo.insert()
  end

  def changeset(struct, params) do
    struct
    |> cast(params, [:resource, :lat, :lon, :accuracy])
    |> validate_required([:resource, :lat, :lon, :accuracy])
    |> validate_number(:accuracy, greater_than_or_equal_to: 0)
  end

  @doc ""
  @spec check_for_bot_events(t, User.t()) :: t
  def check_for_bot_events(%Location{} = loc, user) do
    maybe_do_async(fn ->
      user
      |> User.get_subscriptions()
      |> bots_with_events(user, loc)
      |> Enum.each(&trigger_bot_notification(user, &1))
    end)

    loc
  end

  @doc ""
  @spec update_bot_locations(t, User.t()) :: t
  def update_bot_locations(%Location{} = loc, user) do
    if Application.fetch_env!(:wocky, :enable_follow_me_updates) do
      maybe_do_async(fn ->
        user
        |> User.get_owned_bots_with_follow_me()
        |> Enum.each(
          &Bot.update(&1, %{location: GeoUtils.point(loc.lat, loc.lon)})
        )
      end)
    end

    loc
  end

  defp maybe_do_async(fun) do
    if Application.fetch_env!(:wocky, :async_location_processing) do
      Task.async(fun)
    else
      fun.()
    end
  end

  defp bots_with_events(bots, user, loc) do
    Enum.reduce(bots, [], &check_for_event(&1, user, loc, &2))
  end

  defmacrop log_check_result(user, bot, result) do
    quote do
      :ok =
        Logger.debug(fn ->
          """
          User #{unquote(user).id} #{unquote(result)} the perimeter \
          of #{unquote(bot).id}\
          """
        end)
    end
  end

  defp check_for_event(bot, user, loc, acc) do
    :ok =
      Logger.debug(fn ->
        """
        Checking user #{user.id} for collision with bot #{bot.id} \
        at location (#{loc.lat},#{loc.lon})...\
        """
      end)

    # Don't check bots that are owned by the user
    if bot.user_id == user.id do
      :ok =
        Logger.debug(fn ->
          "Skipping bot #{bot.id} since it is owned by #{user.id}"
        end)

      acc
    else
      bot
      |> Bot.contains?(Map.from_struct(loc))
      |> handle_intersection(user, bot, acc)
    end
  end

  defp handle_intersection(true, user, bot, acc) do
    if should_generate_enter_event(user, bot) do
      log_check_result(user, bot, "has entered")
      BotEvent.insert(user, bot, :enter)
      [{bot, :enter} | acc]
    else
      log_check_result(user, bot, "is within")
      acc
    end
  end

  defp handle_intersection(false, user, bot, acc) do
    if should_generate_exit_event(user, bot) do
      log_check_result(user, bot, "has left")
      BotEvent.insert(user, bot, :exit)
      [{bot, :exit} | acc]
    else
      log_check_result(user, bot, "is outside of")
      acc
    end
  end

  defp should_generate_enter_event(user, bot) do
    case BotEvent.get_last_event_type(user.id, bot.id) do
      nil -> true
      :exit -> true
      :enter -> false
    end
  end

  defp should_generate_exit_event(user, bot) do
    case BotEvent.get_last_event_type(user.id, bot.id) do
      nil -> false
      :exit -> false
      :enter -> true
    end
  end

  defp trigger_bot_notification(user, {bot, event}) do
    jid = user.username |> JID.make(user.server) |> JID.to_binary()
    :ok = Logger.info("User #{jid} #{event}ed the perimeter of bot #{bot.id}")

    if Application.fetch_env!(:wocky, :enable_bot_event_notifications) do
      event = %BotPerimeterEvent{
        user: user,
        bot: bot,
        event: event
      }

      Push.notify_all(bot.user_id, event)
    end
  end
end
