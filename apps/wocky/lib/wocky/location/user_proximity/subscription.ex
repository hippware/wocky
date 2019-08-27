defmodule Wocky.Location.UserProximity.Subscription do
  @moduledoc "Ecto schema module for user proximity subscription"

  use Wocky.Repo.Schema

  alias Wocky.Account.User

  @foreign_key_type :binary_id
  @primary_key false

  schema "user_proximity_subscriptions" do
    field :range, :integer, null: false
    field :cooldown, :integer, null: false
    field :last_notification, :utc_datetime

    timestamps()

    belongs_to :user, User, primary_key: true
    belongs_to :target, User, primary_key: true
  end

  @type t :: %__MODULE__{
          user_id: User.id(),
          target_id: User.id(),
          range: non_neg_integer(),
          cooldown: non_neg_integer(),
          last_notification: DateTime.t()
        }

  @update_fields [
    :range,
    :cooldown,
    :last_notification
  ]

  def changeset(subscription, params) do
    subscription
    |> cast(params, @update_fields)
    |> validate_number(:range, greater_than: 0)
    |> validate_number(:cooldown, greater_than_or_equal_to: 0)
  end
end
