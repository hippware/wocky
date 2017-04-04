defmodule Wocky.Conversation do
  @moduledoc ""

  use Wocky.Repo.Model

  alias Wocky.User
  alias __MODULE__, as: Conversation

  schema "conversations" do
    field :user_id,   :binary_id
    field :other_jid, :string
    field :message,   :binary
    field :outgoing,  :boolean

    timestamps()
  end

  @type t :: %Conversation{
    user_id:    User.id,
    other_jid:  binary,
    message:    binary,
    outgoing:   boolean,
    created_at: DateTime::t,
    updated_at: DateTime::t
  }

  @doc "Write a conversation record to the database"
  @spec put(User.id, binary, binary, boolean) :: :ok
  def put(user_id, other_jid, message, outgoing) do
    %Conversation{
      user_id: user_id,
      other_jid: other_jid,
      message: message,
      outgoing: outgoing}
    |> Repo.insert!(on_conflict: :replace_all)
    :ok
  end

  @spec find(binary) :: [t]
  def find(user_id) do
    Repo.all(from c in Conversation, where: c.user_id == ^user_id,
                                     order_by: [asc: :updated_at])
  end
end
