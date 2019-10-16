defmodule Wocky.Messaging do
  @moduledoc "Context for user-to-user messaging"

  import Ecto.Query

  alias Ecto.Queryable
  alias Wocky.Account.User
  alias Wocky.Messaging.Conversation
  alias Wocky.Messaging.Message
  alias Wocky.Repo
  alias Wocky.Roster

  @spec send_message(User.t(), User.t(), binary, binary | nil, binary | nil) ::
          {:ok, Message.t()} | {:error, any}
  def send_message(recipient, sender, content, image_url, client_data) do
    if can_send?(sender, recipient) do
      %{
        sender_id: sender.id,
        recipient_id: recipient.id,
        content: content,
        image_url: image_url,
        client_data: client_data
      }
      |> Message.changeset()
      |> Repo.insert(returning: true)
    else
      {:error, :permission_denied}
    end
  end

  def mark_read(id, requestor, read \\ true) do
    case id |> received_message_query(requestor) |> Repo.one() do
      nil ->
        {:error, :invalid_id}

      msg ->
        msg
        |> Message.changeset(%{read: read})
        |> Repo.update()

        :ok
    end
  end

  defp can_send?(sender, recipient), do: Roster.friend?(sender, recipient)

  @doc "Query to get all messages to and from the given user"
  @spec get_messages_query(User.t()) :: Queryable.t()
  def get_messages_query(user) do
    Message
    |> where([m], m.sender_id == ^user.id or m.recipient_id == ^user.id)
  end

  @doc "Query to get all messages between the two specified users"
  @spec get_messages_query(User.t(), User.t()) :: Queryable.t()
  def get_messages_query(user, other_user) do
    user
    |> get_messages_query()
    |> where(
      [m],
      m.sender_id == ^other_user.id or m.recipient_id == ^other_user.id
    )
  end

  @spec get_conversations_query(User.id()) :: Queryable.t()
  def get_conversations_query(user_id) do
    Conversation
    |> where(user_id: ^user_id)
  end

  def received_message_query(id, requestor) do
    Message
    |> where([m], m.id == ^id and m.recipient_id == ^requestor.id)
  end

  @spec unread_count(User.t()) :: non_neg_integer()
  def unread_count(requestor) do
    Message
    |> where([m], m.recipient_id == ^requestor.id and not m.read)
    |> select([m], count(1))
    |> Repo.one()
  end
end
