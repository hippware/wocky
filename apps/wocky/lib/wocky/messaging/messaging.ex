defmodule Wocky.Messaging do
  @moduledoc "Context for user-to-user messaging"

  import Ecto.Query

  alias Ecto.Queryable
  alias Wocky.Account.User
  alias Wocky.Messaging.Conversation
  alias Wocky.Messaging.Message
  alias Wocky.Repo
  alias Wocky.Roster

  @spec send_message(User.t(), User.t(), binary, binary | nil) ::
          {:ok, Message.t()} | {:error, any}
  def send_message(recipient, sender, content, image_url \\ nil) do
    if can_send?(sender, recipient) do
      %{
        sender_id: sender.id,
        recipient_id: recipient.id,
        content: content,
        image_url: image_url
      }
      |> Message.changeset()
      |> Repo.insert(returning: true)
    else
      {:error, :permission_denied}
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
end
