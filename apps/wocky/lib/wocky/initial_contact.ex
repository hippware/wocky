defmodule Wocky.InitialContact do
  @moduledoc ""

  use Wocky.Repo.Model
  use Wocky.JID

  import EctoHomoiconicEnum, only: [defenum: 2]

  alias __MODULE__, as: InitialContact
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
    user_id: User.id,
    user: User.t,
    type: type
  }

  @spec get :: [InitialContact.t]
  def get do
    InitialContact
    |> preload(:user)
    |> Repo.all
  end

  @spec put(User.t, type) :: :ok
  def put(user, type) do
    %InitialContact{}
    |> cast(%{user_id: user.id, type: type}, [:user_id, :type])
    |> Repo.insert!(on_conflict: :replace_all, conflict_target: [:user_id])
    :ok
  end
end
