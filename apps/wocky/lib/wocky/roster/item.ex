defmodule Wocky.Roster.Item do
  @moduledoc """
  DB interface module for roster items
  """

  use Wocky.Repo.Schema

  import EctoHomoiconicEnum, only: [defenum: 2]

  alias Wocky.User

  defenum AskEnum, [:in, :out, :both, :none]
  defenum SubscriptionEnum, [:none, :from, :to, :both]

  @foreign_key_type :binary_id
  schema "roster_items" do
    field :name, :binary, default: ""
    field :ask, AskEnum
    field :subscription, SubscriptionEnum
    field :groups, {:array, :string}

    belongs_to :user, User
    belongs_to :contact, User

    timestamps()
  end

  @type name :: binary
  @type ask :: :in | :out | :both | :none
  @type subscription :: :both | :from | :to | :none | :remove
  @type group :: binary

  @type t :: %Item{
          user: User.t(),
          contact: User.t(),
          name: name,
          ask: ask,
          subscription: subscription,
          groups: [group],
          updated_at: DateTime.t()
        }

  @change_fields [:user_id, :contact_id, :name, :ask, :subscription, :groups]

  def changeset(struct, params) do
    struct
    |> cast(params, @change_fields)
    |> foreign_key_constraint(:user_id)
    |> foreign_key_constraint(:contact_id)
  end
end
