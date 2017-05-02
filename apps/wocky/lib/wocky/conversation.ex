defmodule Wocky.Conversation do
  @moduledoc """
  DB interface module for conversations
  """

  use Wocky.Repo.Model

  alias Wocky.User
  alias __MODULE__, as: Conversation

  @foreign_key_type :binary_id
  schema "conversations" do
    field :other_jid, :string
    field :message,   :binary
    field :outgoing,  :boolean

    belongs_to :user, User

    timestamps()
  end

  @type t :: %Conversation{
    id:         integer,
    user_id:    User.id,
    other_jid:  binary,
    message:    binary,
    outgoing:   boolean,
    created_at: DateTime.t,
    updated_at: DateTime.t
  }

  @doc "Write a conversation record to the database"
  @spec put(User.id, binary, binary, boolean) :: :ok
  def put(user_id, other_jid, message, outgoing) do
    conversation = %Conversation{
      user_id: user_id,
      other_jid: other_jid,
      message: message,
      outgoing: outgoing
    }
    Repo.insert!(conversation, on_conflict: :replace_all,
                               conflict_target: [:user_id, :other_jid])
    :ok
  end

  @spec find(binary) :: [t]
  def find(user_id) do
    Repo.all(from c in Conversation, where: c.user_id == ^user_id,
                                     order_by: [asc: :updated_at])
  end
end
