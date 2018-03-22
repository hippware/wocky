defmodule Wocky.User.Location do
  @moduledoc "Interface for user location processing."

  use Wocky.JID
  use Wocky.Repo.Schema

  alias Wocky.Bot
  alias Wocky.GeoUtils
  alias Wocky.Repo
  alias Wocky.User
  alias Wocky.User.BotEvent

  require Logger

  @enter_debounce_seconds 120
  @exit_debounce_seconds 60

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
      |> User.get_guest_subscriptions()
      |> check_for_events(user, loc)
      |> Enum.each(&process_bot_events/1)
    end)

    loc
  end

  defp maybe_do_async(fun) do
    if Application.fetch_env!(:wocky, :async_location_processing) do
      Task.async(fun)
    else
      fun.()
    end
  end

  defp check_for_events(bots, user, loc) do
    Enum.reduce(bots, [], &check_for_event(&1, user, loc, &2))
  end

  defp check_for_event(bot, user, loc, acc) do
    :ok =
      Logger.debug(fn ->
        """
        Checking user #{user.id} for collision with bot #{bot.id} \
        at location (#{loc.lat},#{loc.lon})...\
        """
      end)

    bot
    |> Bot.contains?(Map.from_struct(loc))
    |> handle_intersection(user, bot, acc)
  end

  defp handle_intersection(inside?, user, bot, acc) do
    event = BotEvent.get_last_event(user.id, bot.id)

    case user_state_change(inside?, event) do
      :no_change ->
        acc

      :roll_back ->
        Repo.delete!(event)
        acc

      new_state ->
        new_event = BotEvent.insert(user, bot, new_state)
        [{user, bot, new_event} | acc]
    end
  end

  defp user_state_change(true, nil), do: :transition_in

  defp user_state_change(true, be) do
    case be.event do
      :exit ->
        :transition_in

      :enter ->
        :no_change

      :transition_out ->
        :roll_back

      :transition_in ->
        if debounce_expired?(be.created_at, @enter_debounce_seconds) do
          :enter
        else
          :no_change
        end
    end
  end

  defp user_state_change(false, nil), do: :no_change

  defp user_state_change(false, be) do
    case be.event do
      :exit ->
        :no_change

      :enter ->
        :transition_out

      :transition_in ->
        :roll_back

      :transition_out ->
        if debounce_expired?(be.created_at, @exit_debounce_seconds) do
          :exit
        else
          :no_change
        end
    end
  end

  defp debounce_expired?(ts, wait) do
    Timex.diff(Timex.now(), ts, :seconds) >= wait
  end

  defp process_bot_events({user, bot, be}) do
    if transition_complete?(be) do
      maybe_visit_bot(be.event, user, bot)
    end
  end

  defp transition_complete?(%BotEvent{event: event}) do
    Enum.member?([:enter, :exit], event)
  end

  defp maybe_visit_bot(:enter, user, bot), do: Bot.visit(bot, user)

  defp maybe_visit_bot(:exit, user, bot), do: Bot.depart(bot, user)
end
