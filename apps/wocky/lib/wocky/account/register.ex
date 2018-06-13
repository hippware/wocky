defmodule Wocky.Account.Register do
  @moduledoc "Logic for finding or creating an account post authentication."

  import Ecto.Changeset

  alias Wocky.HomeStream
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.Roster
  alias Wocky.User

  require Logger

  @max_register_retries 5

  @changeset_fields [
    :username,
    :provider,
    :external_id,
    :phone_number,
    :password,
    :pass_details
  ]

  @spec get_external_id(User.t(), binary) :: binary
  def get_external_id(user, provider \\ "firebase") do
    case user.external_id do
      nil ->
        external_id = Factory.external_id()

        {:ok, _} =
          User.update(user, %{
            provider: provider,
            external_id: external_id
          })

        external_id

      external_id ->
        external_id
    end
  end

  @spec find(binary | atom, binary, binary) :: {:ok, User.t()} | {:error, any}
  def find(provider, external_id, phone_number) do
    provider = to_string(provider)

    case Repo.get_by(User, external_id: external_id, provider: provider) do
      nil ->
        case Repo.get_by(User, phone_number: phone_number) do
          nil ->
            {:error, :not_found}

          orig_user ->
            User.update(orig_user, %{
              provider: provider,
              external_id: external_id,
              phone_number: phone_number
            })
        end

      user ->
        {:ok, user}
    end
  end

  @spec create(map(), boolean()) :: {:ok, User.t()} | {:error, any}
  def create(user_data, prepop \\ false) do
    user_data =
      %{
        username: ID.new(),
        provider: "local",
        external_id: Factory.external_id()
      }
      |> Map.merge(user_data)

    case user_data |> changeset() |> Repo.insert() do
      {:ok, user} ->
        if prepop, do: prepopulate_user(user)
        {:ok, user}

      {:error, e} = error ->
        Logger.debug(fn -> "registration failed with error: #{inspect(e)}" end)
        error
    end
  end

  @spec find_or_create(atom | binary, binary, binary) ::
          {:ok, {User.t(), boolean}}
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

    case create(user_data, true) do
      {:ok, user} ->
        {:ok, {user, true}}

      {:error, _} ->
        find_or_create(provider, external_id, phone_number, retries + 1)
    end
  end

  @doc false
  def changeset(attrs) do
    %User{}
    |> cast(attrs, @changeset_fields)
    |> validate_required([:username, :provider, :external_id])
    |> validate_format(:phone_number, ~r//)
    |> validate_change(:username, &validate_username/2)
    |> put_change(:id, attrs[:username])
    |> unique_constraint(:external_id)
  end

  defp validate_username(:username, username) do
    if ID.valid?(username) do
      []
    else
      [username: "not a valid UUID"]
    end
  end

  defp prepopulate_user(user) do
    Roster.add_initial_contacts_to_user(user.id)
    HomeStream.prepopulate(user.id)
  end
end
