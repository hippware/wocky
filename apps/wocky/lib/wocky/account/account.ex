defmodule Wocky.Account do
  @moduledoc "Schema and API for working with users."

  import Ecto.Query

  alias Ecto.Queryable
  alias Wocky.Account.Auth
  alias Wocky.Account.InviteCode
  alias Wocky.Account.User
  alias Wocky.Block
  alias Wocky.Bot
  alias Wocky.Bot.Invitation
  alias Wocky.Bot.Subscription
  alias Wocky.Events.NewUser
  alias Wocky.Notifier
  alias Wocky.Repo
  alias Wocky.Roster
  alias Wocky.TROS

  require Logger

  @invite_code_expire_days 30

  # ----------------------------------------------------------------------
  # Authentication

  defdelegate get_location_jwt(user), to: Auth

  defdelegate authenticate_for_location(token), to: Auth

  defdelegate authenticate(token), to: Auth

  # ----------------------------------------------------------------------
  # Utilities

  @doc "Returns true if the user is a Hippware employee"
  @spec hippware?(User.t()) :: boolean
  def hippware?(%User{email: email}),
    do: email && String.ends_with?(email, "@hippware.com")

  def hippware?(_), do: false

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
    with %User{} = u <- get_user(user.id) do
      if u.smss_sent < Confex.get_env(:wocky, :max_sms_per_user) do
        {:ok, _} = __MODULE__.update(user.id, %{smss_sent: u.smss_sent + 1})
        true
      else
        allowed_unlimited_smss?(user)
      end
    else
      _ -> false
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
  # Bot relationships

  @doc "Returns all bots that the user owns"
  @spec get_owned_bots(User.t()) :: [Bot.t()]
  def get_owned_bots(user) do
    user
    |> owned_bots_query()
    |> order_by(asc: :updated_at)
    |> Repo.all()
  end

  @spec owned_bots_query(User.t()) :: Queryable.t()
  def owned_bots_query(user) do
    user
    |> Ecto.assoc(:bots)
    |> where(pending: false)
  end

  @doc "Returns all bots that the user subscribes to"
  @spec get_subscriptions(User.t()) :: [Bot.t()]
  def get_subscriptions(user) do
    Bot
    |> where(pending: false)
    |> join(
      :left,
      [b],
      s in Subscription,
      on: b.id == s.bot_id and s.user_id == ^user.id
    )
    |> where([b, s], not is_nil(s.user_id))
    |> select([:id, :title, :location])
    |> Repo.all()
  end

  @spec can_access?(User.t(), Bot.t()) :: boolean
  def can_access?(user, bot) do
    owns?(user, bot) || Invitation.invited?(bot, user) ||
      Subscription.state(user, bot) != nil
  end

  defp owns?(user, bot), do: user.id == bot.user_id

  @spec get_bot_relationships(User.t(), Bot.t()) :: [User.bot_relationship()]
  def get_bot_relationships(user, bot) do
    sub = Subscription.get(user, bot)

    [:visible]
    |> maybe_add_rel(bot.user_id == user.id, :owned)
    |> maybe_add_rel(Invitation.invited?(bot, user), :invited)
    |> maybe_add_rel(sub != nil, [:subscribed])
    |> maybe_add_rel(sub != nil && sub.visitor, :visitor)
    |> List.flatten()
  end

  defp maybe_add_rel(list, true, rel), do: [rel | list]
  defp maybe_add_rel(list, false, _rel), do: list

  # ----------------------------------------------------------------------
  # Invite codes

  @spec make_invite_code(User.t()) :: binary()
  def make_invite_code(user) do
    code = InviteCode.generate()

    user
    |> Ecto.build_assoc(:invite_codes)
    |> InviteCode.changeset(%{code: code})
    |> Repo.insert!()

    code
  end

  @spec redeem_invite_code(User.t(), binary()) :: boolean()
  def redeem_invite_code(redeemer, code) do
    invitation =
      InviteCode
      |> where(code: ^code)
      |> preload(:user)
      |> Block.object_visible_query(redeemer)
      |> Repo.one()

    do_redeem_invite_code(redeemer, invitation)
  end

  defp do_redeem_invite_code(_, nil), do: false

  defp do_redeem_invite_code(redeemer, %InviteCode{user: inviter} = invitation),
    do: do_redeem_invite_code(redeemer, inviter, invitation)

  defp do_redeem_invite_code(%User{id: id}, %User{id: id}, _), do: true

  defp do_redeem_invite_code(redeemer, inviter, invitation) do
    ts = Timex.shift(invitation.created_at, days: @invite_code_expire_days)

    if Timex.after?(DateTime.utc_now(), ts) do
      # Code has expired
      false
    else
      :ok = Roster.befriend(redeemer, inviter)
      true
    end
  end

  # ----------------------------------------------------------------------
  # Searching

  @spec search_by_name(binary(), User.t(), non_neg_integer()) :: [User.t()]
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
