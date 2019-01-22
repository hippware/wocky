defmodule Wocky.User.LocationShare do
  @moduledoc false

  use Wocky.Repo.Schema

  alias Wocky.User

  @foreign_key_type :binary_id
  @primary_key {:id, :binary_id, autogenerate: true}
  schema "user_location_shares" do
    field :expires_at, :utc_datetime, null: false

    timestamps()

    belongs_to :user, User
    belongs_to :shared_to, User, foreign_key: :shared_to_id
  end

  @type t :: %LocationShare{
          user_id: User.id(),
          shared_to_id: User.id(),
          expires_at: DateTime.t(),
          created_at: DateTime.t(),
          updated_at: DateTime.t()
        }

  @doc false
  def changeset(struct, params) do
    struct
    |> cast(params, [:user_id, :shared_to_id, :expires_at])
    |> validate_required([:user_id, :shared_to_id, :expires_at])
    |> foreign_key_constraint(:user_id)
    |> foreign_key_constraint(:shared_to_id)
    |> validate_change(:expires_at, fn :expires_at, expiry ->
      if Timex.before?(expiry, Timex.now()) do
        [expires_at: "must be in the future"]
      else
        []
      end
    end)
  end
end
