defmodule Wocky.Contacts.Share.CachedRelationship do
  @moduledoc """
  Structure for the subset of Friend fields held in the Redis cache
  """

  alias Wocky.Account.User
  alias Wocky.Contacts.Relationship

  @fields [
    :contact_id,
    :share_type,
    :nearby_distance,
    :nearby_cooldown,
    :nearby_last_start_notification
  ]

  defstruct @fields

  @type t :: %__MODULE__{
          contact_id: User.id(),
          share_type: Relationship.share_type(),
          nearby_distance: integer(),
          nearby_cooldown: integer(),
          nearby_last_start_notification: DateTime.t()
        }

  def fields, do: @fields

  @spec new(Relationship.t()) :: t()
  def new(relationship) do
    Enum.reduce(@fields, %__MODULE__{}, fn field, acc ->
      Map.put(acc, field, Map.get(relationship, field))
    end)
  end
end
