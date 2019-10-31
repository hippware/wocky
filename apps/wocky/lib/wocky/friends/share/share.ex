defmodule Wocky.Friends.Share do
  @moduledoc false

  # This is a temporary shim to make the old location sharing API work.

  use Ecto.Schema

  import Ecto.Changeset

  alias Wocky.Account.User
  alias Wocky.Friends.Friend
  alias Wocky.Repo
  alias Wocky.Repo.Timestamp

  @primary_key false
  embedded_schema do
    field :id, :integer
    field :user, :map
    field :user_id, :binary_id
    field :shared_with, :map
    field :shared_with_id, :binary_id
    field :share_type, :string
    field :created_at, :utc_datetime_usec
    field :expires_at, :utc_datetime_usec
  end

  @type t :: %__MODULE__{
          id: integer(),
          user: User.t(),
          user_id: User.id(),
          shared_with: User.t(),
          shared_with_id: User.id(),
          share_type: :always | :nearby,
          expires_at: DateTime.t(),
          created_at: DateTime.t()
        }

  @spec make_expiry :: DateTime.t()
  def make_expiry do
    Timestamp.shift(years: 1)
    |> DateTime.truncate(:second)
  end

  @spec make_shim(Friend.t(), DateTime.t()) :: t()
  def make_shim(item, expiry \\ make_expiry()) do
    item = Repo.preload(item, [:user, :contact])

    %__MODULE__{
      id: item.share_id,
      user: item.user,
      user_id: item.user.id,
      shared_with: item.contact,
      shared_with_id: item.contact.id,
      share_type: item.share_type,
      created_at: item.share_changed_at,
      expires_at: expiry
    }
  end

  @spec make_error(map()) :: Ecto.Changeset.t()
  def make_error(params) do
    %__MODULE__{}
    |> change(params)
    |> add_error(:shared_with_id, "must be a friend")
  end
end
