defmodule Wocky.Account do
  @moduledoc """
  API for the Account context.

  This includes user management and authentication.
  """

  use Wocky.Context

  alias Wocky.Account.Auth
  alias Wocky.Account.User
  alias Wocky.Contacts
  alias Wocky.Events.NewUser
  alias Wocky.Notifier
  alias Wocky.TROS

  require Logger

  # ----------------------------------------------------------------------
  # Authentication

  @spec get_location_jwt(User.t()) :: {:ok, String.t()} | {:error, any()}
  defdelegate get_location_jwt(user), to: Auth

  @spec authenticate_for_location(String.t()) ::
          {:ok, User.id()} | {:error, any()}
  defdelegate authenticate_for_location(token), to: Auth

  @spec authenticate(String.t()) :: {:ok, Auth.auth_data()} | {:error, any()}
  defdelegate authenticate(token), to: Auth

  # ----------------------------------------------------------------------
  # Utilities

  @spec first_name(User.t()) :: String.t()
  def first_name(%User{name: name}),
    do: name |> split_name() |> elem(0)

  @spec last_name(User.t()) :: String.t()
  def last_name(%User{name: name}),
    do: name |> split_name() |> elem(1)

  defp split_name(nil), do: {"", ""}
  defp split_name(""), do: {"", ""}

  defp split_name(name) do
    last =
      name
      |> String.split(" ", trim: true)
      |> List.last()

    first =
      name
      |> String.trim()
      |> String.replace_suffix(last, "")
      |> String.trim()

    {first, last}
  end

  # ----------------------------------------------------------------------
  # Database interaction

  @spec get_user(User.id(), User.tid() | nil) :: User.t() | nil
  def get_user(id, requestor \\ nil) do
    if is_nil(requestor) || !Contacts.blocked?(requestor, id) do
      Repo.get(User, id)
    end
  end

  @spec get_by_phone_number([String.t()], User.tid()) :: [User.t()]
  def get_by_phone_number(phone_numbers, requestor) do
    Repo.all(
      from u in Contacts.object_visible_query(User, requestor, :id),
        where: u.phone_number in ^phone_numbers
    )
  end

  @doc """
  Update the data on an existing user.
  Fields is a map containing fields to update.
  """
  @spec update(User.t(), map()) :: Repo.result(User.t())
  def update(user, fields) do
    changeset = User.changeset(user, fields)

    with {:ok, updated_user} <- Repo.update(changeset) do
      maybe_send_welcome(updated_user)
      {:ok, updated_user}
    end
  rescue
    e in Ecto.StaleEntryError ->
      {:error, Exception.message(e)}
  end

  @doc """
  Check if the user is allowed to send an SMS and increment the count
  of those sent
  """
  @spec sms_allowed_inc?(User.t()) :: boolean()
  def sms_allowed_inc?(user) do
    case get_user(user.id) do
      nil ->
        false

      u ->
        if u.smss_sent < Confex.get_env(:wocky, :max_sms_per_user) do
          {:ok, _} = __MODULE__.update(user, %{smss_sent: u.smss_sent + 1})
          true
        else
          allowed_unlimited_smss?(user)
        end
    end
  end

  defp allowed_unlimited_smss?(user) do
    :wocky
    |> Confex.get_env(:unlimited_sms_numbers)
    |> Enum.member?(user.phone_number)
  end

  defp maybe_send_welcome(%User{welcome_sent: true}), do: :ok
  defp maybe_send_welcome(%User{email: nil}), do: :ok

  defp maybe_send_welcome(%User{} = user) do
    Notifier.notify(%NewUser{user: user})
  end

  @doc "Removes the user from the database"
  @spec delete(User.t()) :: :ok
  def delete(user) do
    TROS.delete_all(user)
    Auth.cleanup(user)
    Repo.delete!(user)

    :ok
  rescue
    Ecto.StaleEntryError ->
      :ok
  end

  @doc "Mark the user as having created a bot at some point in their life"
  @spec flag_bot_created(User.t()) :: Repo.result(User.t())
  def flag_bot_created(%{bot_created: true} = user), do: {:ok, user}

  def flag_bot_created(user) do
    user
    |> User.changeset(%{bot_created: true})
    |> Repo.update()
  end

  # ----------------------------------------------------------------------
  # Searching

  @spec search_by_name(String.t(), User.t(), non_neg_integer()) :: [User.t()]
  def search_by_name("", _, _), do: []

  def search_by_name(search_prefix, user, limit) do
    search_term =
      search_cleanup_regex()
      |> Regex.replace(search_prefix, "")
      |> String.split()
      |> Enum.map(&Kernel.<>(&1, ":*"))
      |> Enum.join(" & ")

    Repo.all(
      from u in Contacts.object_visible_query(User, user, :id),
        where:
          u.id != ^user.id and
            fragment(
              """
              users_name_fts(name, handle)
              @@ to_tsquery('simple', unaccent(?))
              """,
              ^search_term
            ),
        limit: ^limit
    )
  end

  defp search_cleanup_regex do
    ~r|[^\-0-9\p{Ll}\p{Lu}\p{Lo}\p{Z}]|u
  end
end
