defmodule Wocky.Conversation do
  @moduledoc ""

  use Wocky.Repo.Model

  import Ecto.Changeset
  import Ecto.Query, only: [from: 2]

  alias Wocky.Repo

  alias __MODULE__, as: Conversation

  @primary_key false
  schema "conversations" do
    field :server,    :string
    field :user,      :string, primary_key: true
    field :other_jid, :string, primary_key: true
    field :message,   :binary
    field :outgoing,  :boolean

    timestamps()
  end

  @type id :: :mod_mam.message_id()

  @type t :: %Conversation{
    server:     binary,
    user:       binary,
    other_jid:  binary,
    message:    binary,
    outgoing:   boolean,
    created_at: DateTime::t,
    updated_at: DateTime::t
  }

  @change_fields [:server, :user, :other_jid, :message, :outgoing]

  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, @change_fields)
  end

  @doc "Write a conversation record to the database"
  @spec put(t) :: :ok
  def put(conversation) do
    conversation
    |> changeset
    |> Repo.insert(on_conflict: :replace_all)
    :ok
  end

  @spec find(binary) :: [t]
  def find(user) do
    Repo.all(from c in Conversation, where: c.user == ^user)
  end
end
