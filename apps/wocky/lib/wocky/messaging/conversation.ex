defmodule Wocky.Messaging.Conversation do
  @moduledoc """
  DB interface module for conversations
  """

  use Wocky.Repo.Schema

  import EctoEnum

  alias Wocky.Account.User

  defenum(MessageDirectionEnum, :message_direction, [
    :incoming,
    :outgoing
  ])

  @foreign_key_type :binary_id
  schema "conversations" do
    field :content, :binary
    field :image_url, :binary
    field :read, :boolean
    field :client_data, :binary
    field :direction, MessageDirectionEnum

    belongs_to :user, User
    belongs_to :other_user, User

    timestamps()
  end

  @type t :: %__MODULE__{
          id: integer(),
          user_id: User.id(),
          other_user_id: User.id(),
          content: String.t(),
          image_url: String.t(),
          read: boolean(),
          client_data: String.t(),
          direction: MessageDirectionEnum.t(),
          created_at: DateTime.t(),
          updated_at: DateTime.t()
        }
end
