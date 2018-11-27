defmodule Wocky.Push.Log do
  @moduledoc "Schema for push notification logs"

  use Wocky.Repo.Schema

  alias Wocky.User

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id
  schema "push_logs" do
    field :device, :string, null: false
    field :token, :string, null: false
    field :message_id, :string
    field :payload, :string
    field :response, :string, null: false
    field :details, :string

    timestamps(updated_at: false)

    belongs_to :user, User
  end

  @type t :: %Log{
          user_id: Wocky.User.id(),
          device: Wocky.User.device(),
          token: binary,
          message_id: binary,
          payload: binary,
          response: binary,
          details: binary
        }

  @insert_attrs [
    :user_id,
    :device,
    :token,
    :message_id,
    :payload,
    :response,
    :details
  ]

  @doc false
  def insert_changeset(attrs) do
    %Log{}
    |> cast(attrs, @insert_attrs)
    |> validate_required(@insert_attrs -- [:message_id, :details])
    |> foreign_key_constraint(:user_id)
  end
end
