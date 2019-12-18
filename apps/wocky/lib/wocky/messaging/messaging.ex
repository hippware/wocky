defmodule Wocky.Messaging do
  @moduledoc "Context for user-to-user messaging"

  use Wocky.Context

  alias Ecto.Queryable
  alias Wocky.Account.User
  alias Wocky.Contacts
  alias Wocky.Messaging.Message

  @spec send_message(
          User.t(),
          User.tid(),
          String.t(),
          String.t() | nil,
          String.t() | nil
        ) :: Repo.result(Message.t())
  def send_message(recipient, sender, content, image_url, client_data) do
    if can_send?(sender, recipient) do
      sender
      |> build_assoc(:sent_messages)
      |> Message.changeset(%{
        recipient_id: User.id(recipient),
        content: content,
        image_url: image_url,
        client_data: client_data
      })
      |> Repo.insert(returning: true)
    else
      {:error, :permission_denied}
    end
  end

  defp can_send?(sender, recipient), do: Contacts.friend?(sender, recipient)

  @spec mark_read(Message.id(), User.t(), boolean()) ::
          :ok | {:error, :invalid_id}
  def mark_read(id, user, read \\ true) do
    case get_received_message(user, id) do
      nil ->
        {:error, :invalid_id}

      msg ->
        msg
        |> Message.changeset(%{read: read})
        |> Repo.update()

        :ok
    end
  end

  defp get_received_message(user, id) do
    Repo.one(
      from m in received_messages(user),
        where: m.id == ^id
    )
  end

  defp received_messages(user), do: Ecto.assoc(user, :received_messages)

  @doc "Query to get all messages to and from the given user"
  @spec get_messages_query(User.tid()) :: Queryable.t()
  def get_messages_query(user) do
    from m in Message,
      where: m.sender_id == ^User.id(user) or m.recipient_id == ^User.id(user),
      preload: [:sender, :recipient]
  end

  @doc "Query to get all messages between the two specified users"
  @spec get_messages_query(User.tid(), User.tid()) :: Queryable.t()
  def get_messages_query(user, other_user) do
    from m in get_messages_query(user),
      where:
        m.sender_id == ^User.id(other_user) or
          m.recipient_id == ^User.id(other_user)
  end

  @spec get_conversations_query(User.t()) :: Queryable.t()
  def get_conversations_query(user) do
    from assoc(user, :conversations),
      preload: [:other_user]
  end

  @spec unread_count(User.t()) :: non_neg_integer()
  def unread_count(user) do
    Repo.one(
      from m in received_messages(user),
        where: not m.read,
        select: count(1)
    )
  end
end
