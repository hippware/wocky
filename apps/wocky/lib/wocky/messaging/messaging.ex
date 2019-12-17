defmodule Wocky.Messaging do
  @moduledoc "Context for user-to-user messaging"

  import Ecto.Query

  alias Ecto.Queryable
  alias Wocky.Account.User
  alias Wocky.Contacts
  alias Wocky.Messaging.Conversation
  alias Wocky.Messaging.Message
  alias Wocky.Repo

  @spec send_message(
          User.tid(),
          User.tid(),
          String.t(),
          String.t() | nil,
          String.t() | nil
        ) :: Repo.result(Message.t())
  def send_message(recipient, sender, content, image_url, client_data) do
    if can_send?(sender, recipient) do
      %{
        sender_id: User.id(sender),
        recipient_id: User.id(recipient),
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

  @spec mark_read(Message.id(), User.tid(), boolean()) ::
          :ok | {:error, :invalid_id}
  def mark_read(id, requestor, read \\ true) do
    case Repo.one(received_message_query(id, requestor)) do
      nil ->
        {:error, :invalid_id}

      msg ->
        msg
        |> Message.changeset(%{read: read})
        |> Repo.update()

        :ok
    end
  end

  defp can_send?(sender, recipient), do: Contacts.friend?(sender, recipient)

  @doc "Query to get all messages to and from the given user"
  @spec get_messages_query(User.tid()) :: Queryable.t()
  def get_messages_query(user) do
    where(
      Message,
      [m],
      m.sender_id == ^User.id(user) or m.recipient_id == ^User.id(user)
    )
  end

  @doc "Query to get all messages between the two specified users"
  @spec get_messages_query(User.tid(), User.tid()) :: Queryable.t()
  def get_messages_query(user, other_user) do
    user
    |> get_messages_query()
    |> where(
      [m],
      m.sender_id == ^User.id(other_user) or
        m.recipient_id == ^User.id(other_user)
    )
  end

  @spec get_conversations_query(User.tid()) :: Queryable.t()
  def get_conversations_query(user) do
    where(Conversation, user_id: ^User.id(user))
  end

  @spec received_message_query(Message.id(), User.tid()) :: Queryable.t()
  def received_message_query(id, requestor) do
    where(Message, [m], m.id == ^id and m.recipient_id == ^User.id(requestor))
  end

  @spec unread_count(User.tid()) :: non_neg_integer()
  def unread_count(requestor) do
    Message
    |> where([m], m.recipient_id == ^User.id(requestor) and not m.read)
    |> select([m], count(1))
    |> Repo.one()
  end
end
