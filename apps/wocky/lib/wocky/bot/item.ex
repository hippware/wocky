defmodule Wocky.Bot.Item do
  @moduledoc "Represents an item published to a bot"

  use Wocky.Repo.Schema

  import Ecto.Query

  alias Ecto.Changeset
  alias Wocky.Bot
  alias Wocky.Repo
  alias Wocky.Repo.ID
  alias Wocky.User

  @foreign_key_type :binary_id
  @primary_key {:id, :binary_id, autogenerate: false}
  schema "bot_items" do
    field :content, :string
    field :image_url, :string

    timestamps()

    belongs_to :bot, Bot
    belongs_to :user, User
  end

  @type id :: binary
  @type t :: %Item{}

  @spec changeset(t, map) :: Changeset.t()
  def changeset(struct, params) do
    struct
    |> cast(params, [:id, :bot_id, :user_id, :content, :image_url])
    |> validate_required([:id, :bot_id, :user_id])
    |> validate_required_inclusion([:content, :image_url])
    |> foreign_key_constraint(:bot_id)
    |> foreign_key_constraint(:user_id)
  end

  defp validate_required_inclusion(changeset, fields) do
    if Enum.any?(fields, &present?(changeset, &1)) do
      changeset
    else
      # Add the error to the first field only since Ecto requires
      # a field name for each error.
      add_error(
        changeset,
        hd(fields),
        "one of these fields must be present: #{inspect(fields)}"
      )
    end
  end

  defp present?(changeset, field) do
    value = get_field(changeset, field)
    value && value != ""
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
    |> where([i], not is_nil(i.image_url))
    |> select([i], count(i.bot_id))
    |> Repo.one()
  end

  @spec get(id, Bot.t()) :: t | nil
  def get(id, bot) do
    Item
    |> where(id: ^id, bot_id: ^bot.id)
    |> Repo.one()
  end

  @spec put(id, Bot.t(), User.t(), binary, binary) ::
          {:ok, t()} | {:error, term}
  def put(id, %{id: bot_id} = bot, %{id: user_id} = user, content, image_url) do
    id_valid? = ID.valid?(id)
    id = if id_valid?, do: id, else: ID.new()

    case id_valid? && Repo.get(Item, id) do
      x when is_nil(x) or x == false ->
        id
        |> do_insert(bot, user, content, image_url)
        |> maybe_update_bot(bot)

      %Item{user_id: ^user_id, bot_id: ^bot_id} = old_item ->
        old_item
        |> do_update(content, image_url)
        |> maybe_update_bot(bot)

      _ ->
        {:error, :permission_denied}
    end
  end

  defp do_update(item, content, image_url) do
    item
    |> changeset(%{content: content, image_url: image_url})
    |> Repo.update()
  end

  defp do_insert(id, bot, user, content, image_url) do
    %Item{}
    |> changeset(%{
      id: id,
      bot_id: bot.id,
      user_id: user.id,
      content: content,
      image_url: image_url
    })
    |> Repo.insert()
  end

  defp maybe_update_bot({:ok, _} = result, bot) do
    Bot.bump_update_time(bot)
    result
  end

  defp maybe_update_bot(result, _), do: result

  @spec delete(Bot.t()) :: :ok
  def delete(bot) do
    bot
    |> Ecto.assoc(:items)
    |> Repo.delete_all()

    :ok
  end

  @spec delete(id, Bot.t(), User.t()) ::
          :ok | {:error, :not_found | :permission_denied}
  def delete(id, %Bot{user_id: user_id} = bot, %User{id: user_id}) do
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

  def delete(id, bot, %User{id: user_id}) do
    case get(id, bot) do
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
  def delete(bot, %User{id: user_id}) do
    bot
    |> Ecto.assoc(:items)
    |> where(user_id: ^user_id)
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
    |> where([i], not is_nil(i.image_url))
  end
end
