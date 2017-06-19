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
    Repo.insert!(conversation, on_conflict: :replace_all,
                               conflict_target: [:user_id, :other_jid])
    :ok
  end

  @spec find(binary) :: [t]
  def find(user_id) do
    with_user(user_id)
    |> order_by(desc: :updated_at)
    |> Repo.all()
  end

  @spec with_user(binary) :: Queryable.t
  def with_user(user_id) do
    Conversation
    |> where(user_id: ^user_id)
  end
end
