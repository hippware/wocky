defmodule Wocky.Roster.InitialContact do
  @moduledoc false

  use Wocky.JID
  use Wocky.Repo.Schema

  import Ecto.Query
  import EctoHomoiconicEnum, only: [defenum: 2]

  alias Wocky.Repo
  alias Wocky.Roster
  alias Wocky.User

  defenum ContactType, [:follower, :followee, :friend]

  @foreign_key_type :binary_id
  @primary_key false
  schema "initial_contacts" do
    field :type, ContactType, null: false

    belongs_to :user, User, primary_key: true

    timestamps()
  end

  @type type :: :follower | :followee | :friend

  @type t :: %InitialContact{
          user_id: User.id(),
          user: User.t(),
          type: type
        }

  @spec get :: [InitialContact.t()]
  def get do
    InitialContact
    |> preload(:user)
    |> Repo.all()
  end

  @spec put(User.t(), type) :: :ok
  def put(user, type) do
    %InitialContact{}
    |> cast(%{user_id: user.id, type: type}, [:user_id, :type])
    |> Repo.insert!(on_conflict: :replace_all, conflict_target: [:user_id])

    :ok
  end

  @spec add_to_user(User.id()) :: :ok
  def add_to_user(user_id) do
    get() |> Enum.each(&set_initial_contact(user_id, &1))
  end

  defp set_initial_contact(user_id, %{user: user, type: :followee}) do
    set_initial_contact(user_id, user, :to, :from)
  end

  defp set_initial_contact(user_id, %{user: user, type: :follower}) do
    set_initial_contact(user_id, user, :from, :to)
  end

  defp set_initial_contact(user_id, %{user: user, type: :friend}) do
    set_initial_contact(user_id, user, :both, :both)
  end

  defp set_initial_contact(user_id, followee, usub, fsub) do
    user_contact = %{
      user_id: user_id,
      contact_id: followee.id,
      name: followee.handle,
      ask: :none,
      subscription: usub,
      groups: ["__welcome__", "__new__"]
    }

    init_contact = %{
      user_id: followee.id,
      contact_id: user_id,
      name: "",
      ask: :none,
      subscription: fsub,
      groups: ["__welcomed__", "__new__"]
    }

    Roster.put(user_contact)
    Roster.put(init_contact)
  end
end
