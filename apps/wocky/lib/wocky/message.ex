defmodule Wocky.Message do
  @moduledoc """
  DB interface module for messages
  """

  use Wocky.Repo.Schema

  import Ecto.Query

  alias Ecto.Queryable
  alias Wocky.{Repo, Roster, User}

  @foreign_key_type :binary_id
  schema "messages" do
    field :content, :string
    field :image_url, :string

    belongs_to :sender, User
    belongs_to :recipient, User

    timestamps(updated_at: false)
  end

  @type t :: %Message{}

  @spec send(User.t(), User.t(), binary, binary) :: {:ok, t()} | {:error, any}
  def send(recipient, sender, content, image_url \\ nil) do
    if can_send?(sender, recipient) do
      %{
        sender_id: sender.id,
        recipient_id: recipient.id,
        content: content,
        image_url: image_url
      }
      |> changeset()
      |> Repo.insert(returning: true)
    else
      {:error, :permission_denied}
    end
  end

  @doc "Query to get all messages to and from the given user"
  @spec get_query(User.t()) :: Queryable.t()
  def get_query(user) do
    Message
    |> where([m], m.sender_id == ^user.id or m.recipient_id == ^user.id)
  end

  @doc "Query to get all messages between the two specified users"
  @spec get_query(User.t(), User.t()) :: Queryable.t()
  def get_query(user, other_user) do
    user
    |> get_query()
    |> where(
      [m],
      m.sender_id == ^other_user.id or m.recipient_id == ^other_user.id
    )
  end

  defp changeset(params) do
    %Message{}
    |> cast(params, [:sender_id, :recipient_id, :content, :image_url])
    |> validate_required([:sender_id, :recipient_id])
    |> validate_required_inclusion([:content, :image_url])
    |> foreign_key_constraint(:sender_id)
    |> foreign_key_constraint(:recipient_id)
  end

  defp can_send?(sender, recipient),
    do: recipient.id |> Roster.get(sender.id) |> Roster.followee?()
end
