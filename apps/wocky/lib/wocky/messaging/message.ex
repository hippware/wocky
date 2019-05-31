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

    belongs_to :sender, User
    belongs_to :recipient, User

    timestamps(updated_at: false)
  end

  @type t :: %Message{}

  def changeset(params) do
    %Message{}
    |> cast(params, [:sender_id, :recipient_id, :content, :image_url])
    |> validate_required([:sender_id, :recipient_id])
    |> validate_required_inclusion([:content, :image_url])
    |> foreign_key_constraint(:sender_id)
    |> foreign_key_constraint(:recipient_id)
  end
end
