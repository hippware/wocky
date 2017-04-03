defmodule Wocky.Conversation do
  @moduledoc ""

  use Wocky.Repo.Model

  alias __MODULE__, as: Conversation

  @primary_key false
  schema "conversations" do
    field :user,      :string, primary_key: true
    field :other_jid, :string, primary_key: true
    field :message,   :binary
    field :outgoing,  :boolean

    timestamps()
  end

  @type id :: :mod_mam.message_id()

  @type t :: %Conversation{
    user:       binary,
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
  def find(user) do
    Repo.all(from c in Conversation, where: c.user == ^user)
  end
end
