defmodule Wocky.Conversation do
  @moduledoc ""

  use Wocky.Repo.Model

  alias __MODULE__, as: Conversation

  schema "conversations" do
    field :user_id,   :binary_id
    field :other_jid, :string
    field :message,   :binary
    field :outgoing,  :boolean

    timestamps()
  end

  @type id :: :mod_mam.message_id()

  @type t :: %Conversation{
    user_id:    binary,
    other_jid:  binary,
    message:    binary,
    outgoing:   boolean,
    created_at: DateTime::t,
    updated_at: DateTime::t
  }

  @doc "Write a conversation record to the database"
  @spec put(t) :: :ok
  def put(conversation) do
    conversation
    |> Repo.insert!(on_conflict: :replace_all)
    :ok
  end

  @spec find(binary) :: [t]
  def find(user_id) do
    Repo.all(from c in Conversation, where: c.user_id == ^user_id)
  end
end
