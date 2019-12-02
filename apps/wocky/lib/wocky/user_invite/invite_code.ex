defmodule Wocky.UserInvite.InviteCode do
  @moduledoc false

  use Wocky.Repo.Schema

  alias Ecto.Changeset
  alias Ecto.UUID
  alias Wocky.Account.User
  alias Wocky.Friends.Friend
  alias Wocky.PhoneNumber

  @foreign_key_type :binary_id
  schema "user_invite_codes" do
    field :code, :string, null: false
    field :phone_number, :string
    field :share_type, Friend.LocationShareTypeEnum

    timestamps(updated_at: false)

    belongs_to :user, User
  end

  @type t :: %__MODULE__{}

  @spec changeset(User.t(), PhoneNumber.t() | nil, Friend.share_type()) ::
          Changeset.t()
  def changeset(user, phone_number, share_type) do
    params = %{phone_number: phone_number, share_type: share_type}

    user
    |> Ecto.build_assoc(:invite_codes)
    |> cast(params, [:user_id, :phone_number, :share_type])
    |> validate_required([:user_id])
    |> foreign_key_constraint(:user_id)
    |> validate_inclusion(:share_type, Friend.share_types())
    |> validate_and_normalise_phone_number(user)
    |> put_change(:code, generate())
  end

  defp validate_and_normalise_phone_number(changeset, user) do
    case get_change(changeset, :phone_number) do
      nil ->
        changeset

      number ->
        with {:ok, cc} <- PhoneNumber.country_code(user.phone_number),
             {:ok, normalised} <- PhoneNumber.normalise(number, cc) do
          put_change(changeset, :phone_number, normalised)
        else
          {:error, _} ->
            add_error(changeset, :phone_number, "invalid")
        end
    end
  end

  @spec generate :: String.t()
  def generate do
    Base.encode64(UUID.bingenerate())
  end
end
