defmodule Wocky.Bots do
  @moduledoc "Schema and API for working with Bots."

  use Elixometer

  import Ecto.Changeset
  import Ecto.Query

  alias Ecto.Queryable
  alias Geocalc.Point
  alias Wocky.Account
  alias Wocky.Account.User
  alias Wocky.Bots.Bot
  alias Wocky.Bots.Item
  alias Wocky.Repo
  alias Wocky.Repo.ID
  alias Wocky.Waiter

  require Logger
  require Record

  # ----------------------------------------------------------------------
  # Database interaction

  @spec get(Bot.id(), boolean()) :: Bot.t() | nil
  def get(id, include_pending \\ false)

  def get(id, include_pending) when is_binary(id) do
    id
    |> get_query(include_pending)
    |> Repo.one()
  end

  @doc false
  def get_query(id, include_pending \\ false) do
    Bot
    |> where(id: ^id)
    |> maybe_filter_pending(not include_pending)
  end

  @doc false
  def maybe_filter_pending(queryable, false), do: queryable

  def maybe_filter_pending(queryable, true),
    do: where(queryable, pending: false)

  @spec preallocate(User.t()) :: Bot.t() | no_return()
  def preallocate(user) do
    params = %{id: ID.new(), user_id: user.id, pending: true}

    %Bot{}
    |> cast(params, [:id, :user_id, :pending])
    |> foreign_key_constraint(:user_id)
    |> Repo.insert!()
  end

  @spec insert(map(), User.t()) :: {:ok, Bot.t()} | {:error, any()}
  def insert(params, requestor) do
    with {:ok, t} <- do_update(%Bot{}, params, &Repo.insert/1) do
      update_counter("bot.created", 1)
      Account.flag_bot_created(requestor)
      {:ok, t}
    end
  end

  @spec update(Bot.t(), map()) :: {:ok, Bot.t()} | {:error, any()}
  def update(bot, params) do
    do_update(bot, params, &Repo.update/1)
  end

  defp do_update(struct, params, op) do
    struct |> Bot.changeset(params) |> op.()
  end

  @spec delete(Bot.t()) :: :ok
  def delete(bot) do
    Repo.delete(bot)
    update_counter("bot.deleted", 1)
    :ok
  end

  @spec sub_setup_event(Bot.t()) :: Waiter.event()
  def sub_setup_event(bot), do: "bot_sub_setup-" <> bot.id

  # ----------------------------------------------------------------------
  # Location

  @spec lat(Bot.t()) :: float()
  def lat(%Bot{location: %Geo.Point{coordinates: {_, lat}}})
      when not is_nil(lat),
      do: lat

  @spec lon(Bot.t()) :: float()
  def lon(%Bot{location: %Geo.Point{coordinates: {lon, _}}})
      when not is_nil(lon),
      do: lon

  @spec location(Bot.t()) :: Point.t()
  def location(bot), do: %{lat: lat(bot), lon: lon(bot)}

  @doc "Returns the bot's distance from the specified location in meters."
  @spec distance_from(Bot.t(), Point.t()) :: float()
  def distance_from(bot, loc), do: Geocalc.distance_between(location(bot), loc)

  @doc "Returns true if the location is within the bot's radius."
  @spec contains?(Bot.t(), Point.t()) :: boolean()
  def contains?(bot, loc), do: Geocalc.within?(bot.radius, location(bot), loc)

  # ----------------------------------------------------------------------
  # Bot items

  @spec get_items(Bot.t()) :: [Item.t()]
  def get_items(bot) do
    bot |> get_items_query() |> Repo.all()
  end

  @spec get_items_query(Bot.t()) :: Queryable.t()
  def get_items_query(bot) do
    Ecto.assoc(bot, :items)
  end

  @spec get_item(Bot.t(), Item.id()) :: Item.t() | nil
  def get_item(bot, id) do
    Item
    |> where(id: ^id, bot_id: ^bot.id)
    |> Repo.one()
  end

  @spec put_item(Bot.t(), Item.id(), binary(), binary(), User.t()) ::
          {:ok, Item.t()} | {:error, any()}
  def put_item(
        %{id: bot_id} = bot,
        id,
        content,
        image_url,
        %{id: user_id} = user
      ) do
    id_valid? = ID.valid?(id)
    id = if id_valid?, do: id, else: ID.new()

    case id_valid? && Repo.get(Item, id) do
      x when is_nil(x) or x == false ->
        id
        |> do_insert_item(bot, user, content, image_url)
        |> maybe_update_bot(bot)

      %Item{user_id: ^user_id, bot_id: ^bot_id} = old_item ->
        old_item
        |> do_update_item(content, image_url)
        |> maybe_update_bot(bot)

      _ ->
        {:error, :permission_denied}
    end
  end

  defp do_update_item(item, content, image_url) do
    item
    |> Item.changeset(%{content: content, image_url: image_url})
    |> Repo.update()
  end

  defp do_insert_item(id, bot, user, content, image_url) do
    %Item{}
    |> Item.changeset(%{
      id: id,
      bot_id: bot.id,
      user_id: user.id,
      content: content,
      image_url: image_url
    })
    |> Repo.insert()
  end

  defp maybe_update_bot({:ok, _} = result, bot) do
    bot
    |> cast(%{updated_at: DateTime.utc_now()}, [:updated_at])
    |> Repo.update!()

    result
  end

  defp maybe_update_bot(result, _), do: result

  @spec delete_item(Bot.t(), Item.id(), User.t()) ::
          :ok | {:error, :not_found | :permission_denied}
  def delete_item(%Bot{user_id: user_id} = bot, id, %User{id: user_id}) do
    {deleted, _} =
      bot
      |> Ecto.assoc(:items)
      |> where(id: ^id)
      |> Repo.delete_all()

    case deleted do
      0 -> {:error, :not_found}
      1 -> :ok
    end
  end

  def delete_item(bot, id, %User{id: user_id}) do
    case get_item(bot, id) do
      %Item{user_id: ^user_id} = item ->
        Repo.delete(item)
        :ok

      nil ->
        {:error, :not_found}

      _ ->
        {:error, :permission_denied}
    end
  end

  @spec delete_items(Bot.t(), User.t()) :: :ok
  def delete_items(bot, %User{id: user_id}) do
    bot
    |> Ecto.assoc(:items)
    |> where(user_id: ^user_id)
    |> Repo.delete_all()

    :ok
  end
end
