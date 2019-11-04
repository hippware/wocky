defmodule WockyAPI.Schema.PresenceTypes do
  @moduledoc """
  Absinthe types for handling user presence
  """

  use WockyAPI.Schema.Notation

  alias WockyAPI.Resolvers.Presence

  # -------------------------------------------------------------------
  # Objects

  enum :presence_status do
    @desc "Online"
    value :online

    @desc "Offline"
    value :offline

    # Maybe other items here such as 'DND'
  end

  @desc "Presence data for a user"
  object :presence do
    @desc "The user's current status"
    field :status, :presence_status

    @desc """
    The time at which this status was generated. Because of the distributed
    nature of the system, it is possible, though unlikely, that presence updates
    may arrive at the client out of order. This field should be used to identify
    and discard stale updates.
    """
    field :updated_at, :datetime
  end

  # -------------------------------------------------------------------
  # Mutations

  input_object :presence_status_input do
    @desc "The status to set for the user's presence"
    field :status, non_null(:presence_status)
  end

  payload_object(:presence_status_payload, :boolean)

  object :presence_mutations do
    @desc "Set the current user's presence status"
    field :presence_status, type: :presence_status_payload do
      arg :input, non_null(:presence_status_input)
      resolve &Presence.set_status/2
      middleware &build_payload/2
    end
  end

  # -------------------------------------------------------------------
  # Subscriptions

  object :presence_subscriptions do
    @desc """
    Recieve an update when a friend's presence status changes
    """
    field :presence, non_null(:user) do
      config fn
        _, %{context: %{current_user: user}} ->
          {:ok,
           topic: Presence.presence_subscription_topic(user.id),
           catchup: fn -> Presence.presence_catchup(user) end}

        _, _ ->
          {:error, "This operation requires an authenticated user"}
      end
    end
  end
end
