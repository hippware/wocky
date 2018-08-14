defmodule Wocky.Bot.Item do
  @moduledoc "Represents an item published to a bot"

  use Wocky.Repo.Schema

  import Ecto.Query
  import SweetXml

  alias Ecto.Changeset
  alias Wocky.Bot
  alias Wocky.Repo
  alias Wocky.User
  alias __MODULE__, as: Item

  @foreign_key_type :binary_id
  @primary_key false
  schema "bot_items" do
    field :id, :string, primary_key: true
    field :bot_id, :binary_id, primary_key: true
    field :stanza, :string
    field :image, :boolean, default: false

    timestamps()

    belongs_to :bot, Bot, define_field: false
    belongs_to :user, User
  end

  @type id :: binary
  @type t :: %Item{}

  @spec changeset(t, map) :: Changeset.t()
  def changeset(struct, params) do
    struct
    |> cast(params, [:id, :bot_id, :user_id, :stanza, :image])
    |> validate_required([:id, :bot_id, :user_id, :stanza])
    |> foreign_key_constraint(:bot_id)
    |> foreign_key_constraint(:user_id)
  end

  @spec get(Bot.t()) :: [t]
  def get(bot) do
    bot
    |> items_query()
    |> order_by(asc: :updated_at)
    |> Repo.all()
  end

  @spec get_count(Bot.t()) :: non_neg_integer
  def get_count(bot) do
    bot
    |> Ecto.assoc(:items)
    |> select([i], count(i.bot_id))
    |> Repo.one()
  end

  @spec get_images(Bot.t()) :: [t]
  def get_images(bot) do
    bot
    |> images_query()
    |> order_by(asc: :updated_at)
    |> Repo.all()
  end

  @spec get_image_count(Bot.t()) :: non_neg_integer
  def get_image_count(bot) do
    bot
    |> Ecto.assoc(:items)
    |> where(image: true)
    |> select([i], count(i.bot_id))
    |> Repo.one()
  end

  @spec get(Bot.t(), id) :: t | nil
  def get(bot, id) do
    Repo.get_by(Item, id: id, bot_id: bot.id)
  end

  @spec put(Bot.t(), User.t(), id, binary) :: {:ok, t()} | {:error, term}
  def put(bot, %{id: user_id} = user, id, stanza) do
    case get(bot, id) do
      nil -> do_put(bot, user, id, stanza)
      %Item{user_id: ^user_id} -> do_put(bot, user, id, stanza)
      _ -> {:error, :permission_denied}
    end
  end

  defp do_put(bot, user, id, stanza) do
    with {:ok, item} <-
           %Item{}
           |> changeset(%{
             id: id,
             bot_id: bot.id,
             user_id: user.id,
             stanza: stanza,
             image: has_image(stanza)
           })
           |> Repo.insert(
             on_conflict: :replace_all,
             conflict_target: [:id, :bot_id],
             returning: true
           ) do
      Bot.bump_update_time(bot)
      {:ok, item}
    end
  end

  @spec delete(Bot.t()) :: :ok
  def delete(bot) do
    bot
    |> Ecto.assoc(:items)
    |> Repo.delete_all()

    :ok
  end

  @spec delete(Bot.t(), id, User.t()) ::
          :ok | {:error, :not_found | :permission_denied}
  def delete(%Bot{user_id: user_id} = bot, id, %User{id: user_id}) do
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

  def delete(bot, id, %User{id: user_id}) do
    case get(bot, id) do
      %Item{user_id: ^user_id} = item ->
        Repo.delete(item)
        :ok

      nil ->
        {:error, :not_found}

      _ ->
        {:error, :permission_denied}
    end
  end

  @spec delete(Bot.t(), User.t()) :: :ok
  def delete(bot, %User{id: id}) do
    bot
    |> Ecto.assoc(:items)
    |> where(user_id: ^id)
    |> Repo.delete_all()

    :ok
  end

  def items_query(bot) do
    bot
    |> Ecto.assoc(:items)
  end

  def images_query(bot) do
    bot
    |> Ecto.assoc(:items)
    |> where(image: true)
  end

  @spec has_image(binary()) :: boolean()
  def has_image(stanza), do: get_image(stanza) != nil

  @spec get_image(binary()) :: binary() | nil
  def get_image(stanza) do
    stanza
    |> parse(quiet: true)
    |> xpath(~x"//image/text()"s)
    |> empty_str_to_nil()
  catch
    # Happens on parse failure - ie, not valid XML
    :exit, _ ->
      nil
  end

  defp empty_str_to_nil(""), do: nil
  defp empty_str_to_nil(s), do: s
end
