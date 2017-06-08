defmodule Wocky.RosterItem do
  @moduledoc """
  DB interface module for roster items
  """

  use Wocky.Repo.Model

  import EctoHomoiconicEnum, only: [defenum: 2]

  alias Wocky.RosterItem.AskEnum
  alias Wocky.RosterItem.SubscriptionEnum
  alias Wocky.User
  alias __MODULE__, as: RosterItem

  require Logger

  defenum AskEnum, [:in, :out, :both, :none]
  defenum SubscriptionEnum, [:none, :from, :to, :both]

  @foreign_key_type :binary_id
  schema "roster_items" do
    field :name,         :binary, default: ""
    field :ask,          AskEnum
    field :subscription, SubscriptionEnum
    field :groups,       {:array, :string}

    belongs_to :user,    User
    belongs_to :contact, User

    timestamps()
  end

  @type name :: binary
  @type ask  :: :in | :out | :both | :none
  @type subscription :: :both | :from | :to | :none | :remove
  @type version :: binary
  @type group :: binary
  @type relationship :: :self | :friend | :follower | :followee | :none

  @type t :: %RosterItem{
    user:          User.t,
    contact:       User.t,
    name:          name,
    ask:           ask,
    subscription:  subscription,
    groups:        [group],

    updated_at: DateTime.t
  }

  @change_fields [:user_id, :contact_id, :name, :ask, :subscription, :groups]
  @blocked_group "__blocked__"

  @doc "Write a roster record to the database"
  @spec put(map)
  :: {:ok, RosterItem.t}
  def put(fields) do
    {:ok,
     %RosterItem{}
     |> changeset(fields)
     |> Repo.insert!(on_conflict: :replace_all,
                     conflict_target: [:user_id, :contact_id])}
  end

  @spec get(User.id) :: [t]
  def get(user_id) do
    RosterItem
    |> with_user(user_id)
    |> preload_contact()
    |> Repo.all
  end

  @spec get(User.id, User.id) :: t | nil
  def get(user_id, contact_id) do
    RosterItem
    |> with_contact(contact_id)
    |> preload_contact()
    |> Repo.get_by(user_id: user_id)
  end

  @spec get_pair(User.id, User.id) :: {t, t} | nil
  def get_pair(a, b) do
    RosterItem
    |> with_pair(a, b)
    |> Repo.all
    |> maybe_sort_pair(a, b)
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

  @spec find_users_with_contact(User.id) :: [User.t]
  def find_users_with_contact(contact_id) do
    RosterItem
    |> with_contact(contact_id)
    |> preload_user()
    |> Repo.all
    |> Enum.map(&Map.get(&1, :user))
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

  @doc "Gets all followers of a user"
  @spec followers(User.id) :: [User.t]
  def followers(user_id) do
    user_id
    |> get()
    |> Enum.filter(&is_follower/1)
    |> Enum.map(&Map.get(&1, :contact))
  end

  @doc "Gets all users being followed by the user"
  @spec following(User.id) :: [User.t]
  def following(user_id) do
    RosterItem
    |> with_contact(user_id)
    |> with_subscriptions([:both, :from])
    |> not_blocked()
    |> preload_user()
    |> Repo.all
    |> Enum.map(&Map.get(&1, :user))
  end

  @spec friends(User.id) :: [User.t]
  def friends(user_id) do
    RosterItem
    |> with_user(user_id)
    |> with_subscriptions([:both])
    |> not_blocked()
    |> preload_contact()
    |> Repo.all
    |> Enum.map(&Map.get(&1, :contact))
  end

  @spec is_friend(User.id, User.id) :: boolean
  def is_friend(user_id, contact_id) do
    user_id |> get(contact_id) |> is_friend
  end

  @spec is_follower(User.id, User.id) :: boolean
  def is_follower(user_id, contact_id) do
    user_id |> get(contact_id) |> is_follower
  end

  @spec relationship(User.id, User.id) :: relationship
  def relationship(a, a), do: :self
  def relationship(a, b) do
    case get_pair(a, b) do
      nil ->
        :none
      {a_to_b, b_to_a} ->
        cond do
          is_friend(a_to_b) ->
            :friend
          is_follower(a_to_b) ->
            :follower
          is_follower(b_to_a) ->
            :followee
          true ->
            :none
        end
    end
  end

  @spec bump_all_versions(User.id) :: :ok
  def bump_all_versions(contact_id) do
    RosterItem
    |> with_contact(contact_id)
    |> Repo.update_all(set: [updated_at: NaiveDateTime.utc_now()])
    :ok
  end

  def blocked_group, do: @blocked_group

  defp with_user(query, user_id) do
    from r in query, where: r.user_id == ^user_id
  end

  defp with_contact(query, contact_id) do
    from r in query, where: r.contact_id == ^contact_id
  end

  defp with_subscriptions(query, sub_types) do
    from q in query, where: q.subscription in ^sub_types
  end

  defp not_blocked(query) do
    from q in query, where: not @blocked_group in q.groups
  end

  defp with_ask(query, ask) do
    from r in query, where: r.ask == ^ask
  end

  defp with_pair(query, a, b) do
    from r in query,
      where: ((r.user_id == ^a and r.contact_id == ^b) or
              (r.user_id == ^b and r.contact_id == ^a))
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

  defp preload_user(query) do
    from r in query, preload: :user
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

  defp maybe_sort_pair([], _, _), do: nil
  defp maybe_sort_pair([first = %RosterItem{user_id: first_id}, second],
                       first_id, _) do
    {first, second}
  end
  defp maybe_sort_pair([first, second], _, _), do: {second, first}
  defp maybe_sort_pair(other_list, a, b) do
    :ok = Logger.warn(
            "Expected a roster pair but got #{other_list} for #{a}, #{b}")
    nil
  end
end
