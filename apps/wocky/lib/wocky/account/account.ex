defmodule Wocky.Account do
  @moduledoc "Schema and API for working with users."

  import Ecto.Query

  alias Wocky.Account.Auth
  alias Wocky.Account.User
  alias Wocky.Block
  alias Wocky.Events.NewUser
  alias Wocky.Notifier
  alias Wocky.Repo
  alias Wocky.TROS

  require Logger

  # ----------------------------------------------------------------------
  # Authentication

  defdelegate get_location_jwt(user), to: Auth

  defdelegate authenticate_for_location(token), to: Auth

  defdelegate authenticate(token), to: Auth

  # ----------------------------------------------------------------------
  # Utilities

  def first_name(%User{name: name}),
    do: name |> split_name() |> elem(0)

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

  @spec get_user(User.id(), User.t() | nil) :: User.t() | nil
  def get_user(id, requestor \\ nil) do
    if is_nil(requestor) || !Block.blocked?(requestor.id, id) do
      Repo.get(User, id)
    end
  end

  @spec get_by_phone_number([String.t()], User.t()) :: [User.t()]
  def get_by_phone_number(phone_numbers, requestor) do
    User
    |> where([u], u.phone_number in ^phone_numbers)
    |> Block.object_visible_query(requestor, :id)
    |> Repo.all()
  end

  # TODO The clause that takes an ID in the first parameter appears to be
  # extraneous at this point
  @doc """
  Update the data on an existing user.
  Fields is a map containing fields to update.
  """
  @spec update(User.id() | User.t(), map) :: {:ok, User.t()} | {:error, term}
  def update(%User{} = user, fields) do
    changeset = User.changeset(user, fields)

    with {:ok, updated_user} <- Repo.update(changeset) do
      maybe_send_welcome(updated_user)
      {:ok, updated_user}
    end
  end

  def update(id, fields) do
    case Repo.get(User, id) do
      nil ->
        {:error, :user_not_found}

      struct ->
        __MODULE__.update(struct, fields)
    end
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
          {:ok, _} = __MODULE__.update(user.id, %{smss_sent: u.smss_sent + 1})
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
  @spec delete(User.id()) :: :ok
  def delete(id) do
    user = Repo.get(User, id)

    if user do
      TROS.delete_all(user)
      Auth.cleanup(user)
      Repo.delete!(user)
    end

    :ok
  end

  @doc "Mark the user as having created a bot at some point in their life"
  @spec flag_bot_created(User.t()) :: :ok
  def flag_bot_created(%{bot_created: true}), do: :ok

  def flag_bot_created(user) do
    user
    |> User.changeset(%{bot_created: true})
    |> Repo.update()

    :ok
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

    User
    |> where(
      fragment(
        """
        users_name_fts(name, handle)
        @@ to_tsquery('simple', unaccent(?))
        """,
        ^search_term
      )
    )
    |> Block.object_visible_query(user, :id)
    |> where([u], u.id != ^user.id)
    |> limit(^limit)
    |> Repo.all()
  end

  defp search_cleanup_regex do
    ~r|[^\-0-9\p{Ll}\p{Lu}\p{Lo}\p{Z}]|u
  end
end
