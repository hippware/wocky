defmodule Wocky.Message do
  @moduledoc """
  DB interface module for messages
  """

  use Wocky.Repo.Schema

  import Ecto.Query
  import SweetXml

  alias Ecto.Queryable
  alias Wocky.{Repo, Roster, User}

  @foreign_key_type :binary_id
  schema "messages" do
    field :message, :string

    belongs_to :sender, User
    belongs_to :recipient, User

    timestamps(updated_at: false)
  end

  @type t :: %Message{}

  @spec send(binary(), User.t(), User.t()) :: {:ok, t()} | {:error, any()}
  def send(message, recipient, sender) do
    if can_send?(sender, recipient) do
      %{message: message, sender_id: sender.id, recipient_id: recipient.id}
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

  @spec get_body(t()) :: binary() | nil
  def get_body(message) do
    with {:ok, doc} <- parse_xml(message.message) do
      doc |> xpath(~x"//xml/body/text()"s)
    end
  end

  @spec get_image(t()) :: binary() | nil
  def get_image(message) do
    with {:ok, doc} <- parse_xml(message.message) do
      doc |> xpath(~x"//xml/image/url/text()"s)
    end
  end

  defp changeset(params) do
    %Message{}
    |> cast(params, [:message, :sender_id, :recipient_id])
    |> foreign_key_constraint(:sender_id)
    |> foreign_key_constraint(:recipient_id)
  end

  defp can_send?(sender, recipient),
    do: Roster.followee?(sender.id, recipient.id)

  defp parse_xml(text) do
    # Wrap the whole body in <xml> tags so we can parse multiple tags at the
    # top level
    {:ok, parse("<xml>" <> text <> "</xml>", quiet: true)}
  catch
    :exit, _ -> nil
  end
end
