defmodule Wocky.Messaging.Message do
  @moduledoc """
  DB interface module for messages
  """

  use Wocky.Repo.Schema

  alias Wocky.Account.User

  @foreign_key_type :binary_id
  schema "messages" do
    field :content, :string
    field :image_url, :string
    field :client_data, :string
    field :read, :boolean

    belongs_to :sender, User
    belongs_to :recipient, User

    timestamps()
  end

  @type t :: %Message{}

  def changeset(struct \\ %Message{}, params) do
    struct
    |> cast(params, [:sender_id, :recipient_id, :content, :image_url, :read])
    |> validate_required([:sender_id, :recipient_id])
    |> validate_required_inclusion([:content, :image_url])
    |> foreign_key_constraint(:sender_id)
    |> foreign_key_constraint(:recipient_id)
  end
end
