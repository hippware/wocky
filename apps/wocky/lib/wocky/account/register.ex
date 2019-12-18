defmodule Wocky.Account.Register do
  @moduledoc "Logic for finding or creating an account post authentication."
  use Elixometer

  import Ecto.Changeset
  import Ecto.Query, only: [from: 2]

  alias Ecto.Changeset
  alias Wocky.Account
  alias Wocky.Account.User
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID

  require Logger

  @max_register_retries 5

  @changeset_fields [
    :id,
    :provider,
    :external_id,
    :phone_number,
    :password,
    :pass_details
  ]

  @spec get_external_id(User.t(), String.t()) ::
          {:ok, String.t()} | Repo.error()
  def get_external_id(user, provider \\ "firebase") do
    case user.external_id do
      nil ->
        external_id = Factory.external_id()
        params = %{provider: provider, external_id: external_id}

        with {:ok, _} <- Account.update(user, params) do
          {:ok, external_id}
        end

      external_id ->
        {:ok, external_id}
    end
  end

  defp find_by_phone_or_external_id(phone_number, external_id, provider) do
    Repo.one(
      from u in User,
        where:
          (u.external_id == ^external_id and u.provider == ^provider) or
            u.phone_number == ^phone_number
    )
  end

  @spec find(String.t() | atom(), String.t(), String.t()) ::
          {:ok, User.t()} | {:error, any()}
  def find(provider, external_id, phone_number) do
    provider = to_string(provider)

    case find_by_phone_or_external_id(phone_number, external_id, provider) do
      nil ->
        {:error, :not_found}

      orig_user ->
        Account.update(orig_user, %{
          provider: provider,
          external_id: external_id,
          phone_number: phone_number
        })
    end
  end

  @spec create(map()) :: {:ok, User.t()} | {:error, any()}
  def create(user_data) do
    user_data =
      %{
        id: ID.new(),
        provider: "local",
        external_id: Factory.external_id()
      }
      |> Map.merge(user_data)

    case user_data |> changeset() |> Repo.insert() do
      {:ok, user} ->
        update_counter("user.create", 1)
        {:ok, user}

      {:error, e} = error ->
        Logger.debug(fn -> "registration failed with error: #{inspect(e)}" end)
        error
    end
  end

  @spec find_or_create(atom() | String.t(), String.t(), String.t()) ::
          {:ok, {User.t(), boolean()}}
  def find_or_create(provider, external_id, phone_number) do
    find_or_create(provider, external_id, phone_number, 0)
  end

  # There's scope for a race condition here. Since the database uses "read
  # committed"
  # (see https://www.postgresql.org/docs/current/static/transaction-iso.html)
  # we can't SELECT then INSERT and assume something hasn't been inserted
  # in the interim, even in a transaction. Thus we implement a retry system
  # here - if the user was inserted after the SELECT, the next time around it
  # should work fine (unless it gets deleted in the interim in which case
  # what the heck is even going on?). Either way, if we fail after 5 retries
  # there's something seriously weird going on and raising an exception
  # is a pretty reasonable response.
  defp find_or_create(_, _, _, @max_register_retries) do
    raise "Exceeded maximum registration retries"
  end

  defp find_or_create(provider, external_id, phone_number, retries) do
    case find(provider, external_id, phone_number) do
      {:ok, user} ->
        {:ok, {user, false}}

      {:error, :not_found} ->
        create_new(provider, external_id, phone_number, retries)
    end
  end

  defp create_new(provider, external_id, phone_number, retries) do
    user_data = %{
      provider: to_string(provider),
      external_id: external_id,
      phone_number: phone_number
    }

    case create(user_data) do
      {:ok, user} ->
        {:ok, {user, true}}

      {:error, _} ->
        find_or_create(provider, external_id, phone_number, retries + 1)
    end
  end

  @doc false
  @spec changeset(map()) :: Changeset.t()
  def changeset(attrs) do
    %User{}
    |> cast(attrs, @changeset_fields)
    |> validate_required([:id, :provider, :external_id])
    |> validate_format(:phone_number, ~r//)
    |> validate_change(:id, &validate_id/2)
    |> unique_constraint(:external_id)
  end

  defp validate_id(:id, id) do
    if ID.valid?(id) do
      []
    else
      [id: "not a valid UUID"]
    end
  end
end
