defmodule Wocky.RosterItem do
  @moduledoc """
  DB interface module for roster items
  """

  use Wocky.Repo.Model

  alias Wocky.RosterItem.AskEnum
  alias Wocky.RosterItem.SubscriptionEnum
  alias Wocky.User
  alias __MODULE__, as: RosterItem

  @foreign_key_type :binary_id
  schema "roster_items" do
    field :nick,         :binary
    field :ask,          AskEnum
    field :subscription, SubscriptionEnum
    field :groups,       :binary

    belongs_to :user,    User
    belongs_to :contact, User

    timestamps()
  end

  @type nick :: binary
  @type ask  :: :in | :out | :both | :none
  @type subscription :: :both | :from | :to | :none | :remove
  @type version :: integer
  @type group :: binary

  @type t :: %RosterItem{
    user_id:       User.id,
    contact:       User::t,
    nick:          nick,
    ask:           ask,
    subscription:  subscription,
    groups:        [group],

    updated_at: DateTime::t
  }

  @doc "Write a conversation record to the database"
  @spec put(User.id, User.id, nick, [group], ask, subscription) :: :ok
  def put(user_id, contact_id, nick, groups, ask, subscription) do
    item = %RosterItem{
      user_id: user_id,
      contact_id: contact_id,
      nick: nick,
      ask: ask,
      subscription: subscription,
      groups: serialise_groups(groups),
    }
    Repo.insert!(item, on_conflict: :replace_all)
    :ok
  end

  @spec find(User.id) :: [t]
  def find(user_id) do
    RosterItem
    |> with_user(user_id)
    |> preload_contact()
    |> Repo.all
    |> Enum.map(&deserialise/1)
  end

  @spec find(User.id, User.id) :: t | nil
  def find(user_id, contact_id) do
    RosterItem
    |> with_user(user_id)
    |> with_contact(contact_id)
    |> preload_contact()
    |> Repo.one
    |> deserialise()
  end

  @spec version(User.id) :: version
  def version(user_id) do
    RosterItem
    |> with_user(user_id)
    |> select_version()
    |> Repo.one
    |> normalise_version()
  end

  @spec delete(User.id, User.id) :: :ok
  def delete(user_id, contact) do
    RosterItem
    |> with_user(user_id)
    |> Repo.delete_all
    :ok
  end

  @spec users_with_contact(User.id) :: [User.id]
  def users_with_contact(contact_id) do
    RosterItem
    |> with_contact(contact_id)
    |> select_user()
    |> Repo.all
  end

  defp with_user(query, user_id) do
    from r in query, where: r.user_id == ^user_id
  end

  defp with_contact(query, contact_id) do
    from r in query, where: r.contact_id == ^contact_id
  end

  defp select_user(query) do
    from r in query, select: r.user_id
  end

  defp select_version(query) do
    from r in query, select: max(r.updated_at)
  end

  defp preload_contact(query) do
    from r in query, preload: :contact
  end

  defp normalise_version(nil), do: 0
  defp normalise_version(ver), do: Timex.to_gregorian_microseconds(ver)

  defp serialise_groups(groups) do
    Enum.join(groups, <<0>>)
  end

  defp deserialise(nil), do: nil
  defp deserialise(item) do
    Map.put(item, :groups, deserialise_groups(item.groups))
  end

  defp deserialise_groups(groups) do
    String.split(groups, <<0>>)
  end
end
