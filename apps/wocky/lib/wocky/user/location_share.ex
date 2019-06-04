defmodule Wocky.User.LocationShare do
  @moduledoc false

  import Ecto.Query

  use Wocky.Repo.Schema

  alias Ecto.{Changeset, Queryable}
  alias Wocky.{Repo, Roster, User}
  alias Wocky.User.LocationShare.Cache

  @foreign_key_type :binary_id
  schema "user_location_shares" do
    field :expires_at, :utc_datetime, null: false

    timestamps()

    belongs_to :user, User
    belongs_to :shared_with, User, foreign_key: :shared_with_id
  end

  @type t :: %LocationShare{
          user_id: User.id(),
          shared_with_id: User.id(),
          expires_at: DateTime.t(),
          created_at: DateTime.t(),
          updated_at: DateTime.t()
        }

  @spec start_sharing_location(User.t(), User.t(), DateTime.t()) ::
          {:ok, LocationShare.t()} | {:error, Changeset.t() | atom}
  def start_sharing_location(user, shared_with, expiry) do
    result =
      %LocationShare{}
      |> changeset(%{
        user_id: user.id,
        shared_with_id: shared_with.id,
        expires_at: expiry
      })
      |> Repo.insert(
        on_conflict: [set: [expires_at: expiry, updated_at: DateTime.utc_now()]],
        conflict_target: [:user_id, :shared_with_id]
      )

    result
  end

  @spec stop_sharing_location(User.t(), User.t()) :: :ok
  def stop_sharing_location(%User{id: user_id}, %User{id: shared_with_id}) do
    LocationShare
    |> where(user_id: ^user_id, shared_with_id: ^shared_with_id)
    |> Repo.delete_all()

    :ok
  end

  @spec stop_sharing_location(User.t()) :: :ok
  def stop_sharing_location(%User{id: user_id}) do
    LocationShare
    |> where(user_id: ^user_id)
    |> Repo.delete_all()

    :ok
  end

  @spec get_location_share_targets(User.t()) :: [User.id()]
  def get_location_share_targets(user), do: Cache.get(user.id)

  @spec get_location_shares(User.t()) :: [LocationShare.t()]
  def get_location_shares(user) do
    user
    |> get_location_shares_query()
    |> Repo.all()
  end

  @spec get_location_shares_query(User.t()) :: Queryable.t()
  def get_location_shares_query(%User{id: user_id}) do
    location_shares_query()
    |> where([ls], ls.user_id == ^user_id)
  end

  @spec get_location_sharers(User.t()) :: [LocationShare.t()]
  def get_location_sharers(user) do
    user
    |> get_location_sharers_query()
    |> Repo.all()
  end

  @spec get_location_sharers_query(User.t()) :: Queryable.t()
  def get_location_sharers_query(%User{id: user_id}) do
    location_shares_query()
    |> where([ls], ls.shared_with_id == ^user_id)
  end

  defp location_shares_query do
    now = DateTime.utc_now()

    LocationShare
    |> preload([:user, :shared_with])
    |> where([ls], ls.expires_at > ^now)
    |> order_by([ls], desc: ls.created_at)
  end

  defp changeset(struct, params) do
    struct
    |> cast(params, [:user_id, :shared_with_id, :expires_at])
    |> validate_required([:user_id, :shared_with_id, :expires_at])
    |> foreign_key_constraint(:user_id)
    |> foreign_key_constraint(:shared_with_id)
    |> validate_change(:expires_at, fn :expires_at, expiry ->
      if Timex.before?(expiry, Timex.now()) do
        [expires_at: "must be in the future"]
      else
        []
      end
    end)
    |> validate_change(:shared_with_id, fn :shared_with_id, shared_with_id ->
      if Roster.friend?(params[:user_id], shared_with_id) do
        []
      else
        [shared_with_id: "must be a friend"]
      end
    end)
  end

  @spec clean_expired() :: {non_neg_integer(), [User.id()]}
  def clean_expired do
    {count, user_ids} =
      LocationShare
      |> select([l], l.user_id)
      |> where([l], l.expires_at <= ^DateTime.utc_now())
      |> Repo.delete_all(timeout: :infinity)

    {count, Enum.uniq(user_ids)}
  end
end
