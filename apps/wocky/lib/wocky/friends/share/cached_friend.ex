defmodule Wocky.Friends.Share.CachedFriend do
  @moduledoc """
  Structure for the subset of Friend fields held in the Redis cache
  """

  alias Wocky.Account.User
  alias Wocky.Friends.Friend

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
          share_type: Friend.share_type(),
          nearby_distance: integer(),
          nearby_cooldown: integer(),
          nearby_last_start_notification: DateTime.t()
        }

  def fields, do: @fields
end
