defmodule Wocky.Conversation do
  @moduledoc """
  DB interface module for conversations
  """

  use Wocky.Repo.Model

  alias Ecto.Queryable
  alias Wocky.JID
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

  @type mam_id :: non_neg_integer

  @type t :: %Conversation{
    id:         integer,
    user_id:    User.id,
    other_jid:  binary,
    message:    binary,
    outgoing:   boolean,
    created_at: DateTime.t,
    updated_at: DateTime.t
  }

  @change_fields [:message, :outgoing]

  @doc "Write a conversation record to the database"
  @spec put(mam_id, User.id, binary, binary, boolean) :: :ok
  def put(id, user_id, other_jid, message, outgoing) do
    conversation = %Conversation{
      id: id,
      user_id: user_id,
      other_jid: other_jid,
      message: message,
      outgoing: outgoing
    }
    Repo.insert!(
      conversation,
      on_conflict: [set: conversation
                         |> Map.take(@change_fields)
                         |> Map.put(:updated_at, DateTime.utc_now)
                         |> Map.to_list],
      conflict_target: [:user_id, :other_jid])
    :ok
  end

  @spec get_id(User.id, JID.literal_jid) :: integer | nil
  def get_id(user_id, other_jid) do
    conversation =
      Repo.get_by(
        Conversation,
        user_id: user_id,
        other_jid: other_jid
      )

    case conversation do
      nil -> nil
      c -> c.id
    end
  end

  @spec find(User.id) :: [t]
  def find(user_id) do
    user_id
    |> with_user()
    |> order_by(desc: :updated_at)
    |> Repo.all()
  end

  @spec with_user(User.id) :: Queryable.t
  def with_user(user_id) do
    Conversation
    |> where(user_id: ^user_id)
  end

  @spec delete_user_pair(User.t, User.t) :: :ok
  def delete_user_pair(a, b) do
    other_jid =
      b
      |> User.to_jid
      |> JID.to_binary

    Conversation
    |> where([c], c.user_id == ^a.id and c.other_jid == ^other_jid)
    |> Repo.delete_all

    :ok
  end
end
