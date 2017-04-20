defmodule Wocky.RosterItem do
  @moduledoc """
  DB interface module for roster items
  """

  use Wocky.Repo.Model

  import EctoHomoiconicEnum, only: [defenum: 2]

  alias Wocky.RosterItem.AskEnum
  alias Wocky.RosterItem.SubscriptionEnum
  alias Wocky.StringList
  alias Wocky.User
  alias __MODULE__, as: RosterItem

  defenum AskEnum, [:in, :out, :both, :none]
  defenum SubscriptionEnum, [:none, :from, :to, :both]

  @foreign_key_type :binary_id
  schema "roster_items" do
    field :name,         :binary
    field :ask,          AskEnum
    field :subscription, SubscriptionEnum
    field :groups,       StringList

    belongs_to :user,    User
    belongs_to :contact, User

    timestamps()
  end

  @type name :: binary
  @type ask  :: :in | :out | :both | :none
  @type subscription :: :both | :from | :to | :none | :remove
  @type version :: binary
  @type group :: binary

  @type t :: %RosterItem{
    user_id:       User.id,
    contact:       User::t,
    name:          name,
    ask:           ask,
    subscription:  subscription,
    groups:        [group],

    updated_at: DateTime::t
  }

  @change_fields [:contact_id, :name, :ask, :subscription, :groups]
  @blocked_group "__blocked__"

  @doc "Write a conversation record to the database"
  @spec put(User.id, User.id, name, [group], ask, subscription) :: :ok
  def put(user_id, contact_id, name, groups, ask, subscription) do
    fields = %{
      contact_id: contact_id,
      name: name,
      ask: ask,
      subscription: subscription,
      groups: groups
    }
    %RosterItem{user_id: user_id}
    |> changeset(fields)
    |> Repo.insert!(on_conflict: :replace_all)
    :ok
  end

  @spec find(User.id) :: [t]
  def find(user_id) do
    RosterItem
    |> with_user(user_id)
    |> preload_contact()
    |> Repo.all
  end

  @spec find(User.id, User.id) :: t | nil
  def find(user_id, contact_id) do
    RosterItem
    |> with_user(user_id)
    |> with_contact(contact_id)
    |> preload_contact()
    |> Repo.one
  end

  @spec version(User.id) :: version
  def version(user_id) do
    timestamp =
      RosterItem
      |> with_user(user_id)
      |> select_version()
      |> Repo.one

    count =
      RosterItem
      |> with_user(user_id)
      |> count()
      |> Repo.one

    make_version(timestamp, count)
  end

  @spec delete(User.id) :: :ok
  def delete(user_id) do
    RosterItem
    |> with_user(user_id)
    |> Repo.delete_all
    :ok
  end

  @spec delete(User.id, User.id) :: :ok
  def delete(user_id, contact_id) do
    RosterItem
    |> with_user(user_id)
    |> with_contact(contact_id)
    |> Repo.delete_all
    :ok
  end

  @spec find_users_with_contact(User.id) :: [User.id]
  def find_users_with_contact(contact_id) do
    RosterItem
    |> with_contact(contact_id)
    |> select_user()
    |> Repo.all
  end

  @spec has_contact(User.id, User.id) :: boolean
  def has_contact(user_id, contact_id) do
    RosterItem
    |> with_user(user_id)
    |> with_contact(contact_id)
    |> with_ask(:none)
    |> count()
    |> Repo.one
    |> Kernel.!=(0)
  end

  @spec followers(User.id) :: [t]
  def followers(user_id) do
    user_id
    |> find()
    |> Enum.filter(&is_follower/1)
  end

  @spec friends(User.id) :: [t]
  def friends(user_id) do
    user_id
    |> find()
    |> Enum.filter(&is_friend/1)
  end

  @spec is_friend(User.id, User.id) :: boolean
  def is_friend(user_id, contact_id) do
    user_id |> find(contact_id) |> is_friend
  end

  @spec is_follower(User.id, User.id) :: boolean
  def is_follower(user_id, contact_id) do
    user_id |> find(contact_id) |> is_follower
  end

  @spec bump_version(User.id, User.id) :: :ok
  def bump_version(user_id, contact_id) do
    user_id
    |> find(contact_id)
    |> version_bump_changeset()
    |> Repo.update(force: true)
    :ok
  end

  defp with_user(query, user_id) do
    from r in query, where: r.user_id == ^user_id
  end

  defp with_contact(query, contact_id) do
    from r in query, where: r.contact_id == ^contact_id
  end

  defp with_ask(query, ask) do
    from r in query, where: r.ask == ^ask
  end

  defp select_user(query) do
    from r in query, select: r.user_id
  end

  defp select_version(query) do
    from r in query, select: max(r.updated_at)
  end

  defp count(query) do
    from r in query, select: count(r.contact_id)
  end

  defp preload_contact(query) do
    from r in query, preload: :contact
  end

  defp make_version(nil, count) do
    normalise_version("0", count)
  end
  defp make_version(ver, count) do
    ver
    |> Timex.to_gregorian_microseconds
    |> Integer.to_string
    |> normalise_version(count)
  end

  defp normalise_version(verstr, count) do
    verstr <> "-" <> Integer.to_string(count)
  end

  defp is_friend(%RosterItem{subscription: :both,
                             groups: groups}) do
    ! Enum.member?(groups, @blocked_group)
  end
  defp is_friend(_), do: false

  defp is_follower(%RosterItem{subscription: subscription,
                               groups: groups}) do
    (subscription == :both || subscription == :from)
    &&
    ! Enum.member?(groups, @blocked_group)
  end
  defp is_follower(_), do: false

  defp changeset(struct, params) do
    cast(struct, params, @change_fields)
  end

  defp version_bump_changeset(struct) do
    cast(struct, %{}, [:user_id])
  end

end
