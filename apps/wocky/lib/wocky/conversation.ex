defmodule Wocky.Conversation do
  @moduledoc """
  DB interface module for conversations
  """

  use Wocky.Repo.Schema

  import Ecto.Query
  import EctoHomoiconicEnum, only: [defenum: 2]

  alias Ecto.Queryable
  alias Wocky.User
  alias __MODULE__

  defenum MessageDirection, [
    :incoming,
    :outgoing
  ]

  @foreign_key_type :binary_id
  schema "conversations" do
    field :message, :binary
    field :direction, MessageDirection

    belongs_to :user, User
    belongs_to :other_user, User

    timestamps(updated_at: false)
  end

  @type t :: %Conversation{
          id: integer,
          user_id: User.id(),
          other_user_id: User.id(),
          message: binary,
          direction: MessageDirection,
          created_at: DateTime.t()
        }

  @spec by_user_query(User.id()) :: Queryable.t()
  def by_user_query(user_id) do
    Conversation
    |> where(user_id: ^user_id)
  end
end
