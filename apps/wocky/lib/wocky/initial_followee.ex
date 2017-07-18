defmodule Wocky.InitialFollowee do
  @moduledoc ""

  use Wocky.Repo.Model
  use Wocky.JID

  alias __MODULE__, as: InitialFollowee
  alias Wocky.User

  @foreign_key_type :binary_id
  @primary_key false
  schema "initial_followees" do
    belongs_to :user, User, primary_key: true

    timestamps()
  end

  @type t :: %InitialFollowee{}

  @spec get :: [User.t]
  def get do
    InitialFollowee
    |> preload(:user)
    |> Repo.all
    |> Enum.map(&Map.get(&1, :user))
  end
end
