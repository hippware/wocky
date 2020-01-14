defmodule WockyAPI.Schema.ContactTypes do
  @moduledoc """
  Absinthe types for user friends
  """

  use WockyAPI.Schema.Notation

  alias WockyAPI.Resolvers.Contact

  # -------------------------------------------------------------------
  # Objects

  enum :user_contact_relationship do
    @desc "The parent user has invited the child user to be a friend"
    value :invited

    @desc "The child user has invited the user to be a friend"
    value :invited_by

    @desc "The two users are friends"
    value :friend

    @desc "The users have no relationship"
    value :none

    @desc "The user is the requesting user"
    value :self
  end

  enum :friend_share_type do
    @desc "Always share location to this contact (except when hidden)"
    value :always

    @desc """
    Share location to this contact only when within the specified distance
    from them
    """
    value :nearby

    @desc "Never share location to this contact"
    value :disabled
  end

  object :friend_share_config do
    @desc "Range in meters within which 'nearby' sharing will activate"
    field :nearby_distance, :integer

    @desc "Cooldown period between 'nearby' sharing notifications"
    field :notify_cooldown, :integer
  end

  @desc "Another user with whom a friendship exists"
  object :friend do
    @desc "The friend"
    field :user, non_null(:user), resolve: &Contact.get_contact_user/3

    @desc "DEPRECATED Always returns FRIEND"
    field :relationship, :user_contact_relationship,
      deprecate: "Always returns FRIEND."

    @desc "DEPRECATED Always returns an empty string"
    field :name, :string,
      deprecate: "Always returns an empty string",
      resolve: fn _, _ -> {:ok, ""} end

    @desc "The current user's share level with the other user"
    field :share_type, non_null(:friend_share_type)

    @desc "The current user's location share config for the other user"
    field :share_config, :friend_share_config,
      resolve: fn f, _, _ ->
        {:ok,
         %{
           nearby_distance: f.nearby_distance,
           notify_cooldown: f.nearby_cooldown
         }}
      end

    @desc "The creation time of the friendship"
    field :created_at, non_null(:datetime)
  end

  @desc "An invitation from one user to another to become a friend"
  object :invitation do
    @desc "The sender"
    field :sender, :user, resolve: dataloader(Wocky, :user)

    @desc "The recipient"
    field :recipient, :user, resolve: dataloader(Wocky, :contact)

    @desc "Share mode"
    field :share_type, non_null(:friend_share_type)

    @desc "When the invitation was created"
    field :created_at, non_null(:datetime)
  end

  @desc "A user location update entry"
  object :location do
    @desc "Unique ID of the location report"
    field :id, :uuid

    @desc "Latitude in degrees"
    field :lat, non_null(:float)

    @desc "Longitude in degrees"
    field :lon, non_null(:float)

    @desc "Reported accuracy in meters"
    field :accuracy, non_null(:float)

    @desc "The reported activity for this location update"
    field :activity, :string

    @desc "The confidence value for the reported activity"
    field :activity_confidence, :integer

    @desc "Timestamp when the report was captured on the device"
    field :captured_at, :datetime

    @desc "Time of location report"
    field :created_at, non_null(:datetime)
  end

  @desc "Describes a relationship change with another user"
  object :contact_relationship_change do
    @desc "The other user"
    field :user, non_null(:user), resolve: &Contact.get_contact_user/3

    @desc "The current user's relationship with the other user"
    field :relationship, :user_contact_relationship

    @desc "DEPRECATED Always returns an empty string"
    field :name, :string,
      deprecate: "Always returns an empty string",
      resolve: fn _, _ -> {:ok, ""} end

    @desc "The creation time of the contact"
    field :created_at, non_null(:datetime),
      deprecate: """
      This field can be misleading, especially when relationship is NONE.
      """
  end

  @desc "Data that is sent when a user's shared location changes"
  object :user_location_update do
    @desc "The user whose location has changed"
    field :user, non_null(:user)

    @desc "The user's new location"
    field :location, non_null(:location)
  end

  @desc "Attributes of a user location live sharing session"
  object :user_location_live_share do
    @desc "ID for this sharing session"
    field :id, non_null(:aint), deprecate: "This field will be removed"

    @desc "The user sharing their location"
    field :user, non_null(:other_user)

    @desc "The user with whom the location is being shared"
    field :shared_with, non_null(:other_user)

    @desc "The share mode"
    field :share_type, non_null(:friend_share_type)

    @desc "When the share was created"
    field :created_at, non_null(:datetime)

    @desc "The expiry for the share"
    field :expires_at, non_null(:datetime),
      deprecate: "Expiration is no longer supported"
  end

  # -------------------------------------------------------------------
  # Connections

  connection :friends, node_type: :friend do
    total_count_field()

    edge do
    end
  end

  connection :invitations, node_type: :invitation do
    total_count_field()

    edge do
    end
  end

  connection :user_location_live_shares, node_type: :user_location_live_share do
    total_count_field()

    edge do
    end
  end

  # -------------------------------------------------------------------
  # Mutations

  input_object :friend_invite_input do
    @desc "The ID of the user to invite to be a friend"
    field :user_id, non_null(:uuid)

    @desc "The share mode for the invited user (defaults to DISABLED)"
    field :share_type, :friend_share_type
  end

  payload_object(:friend_invite_payload, :user_contact_relationship)

  input_object :friend_share_config_input do
    @desc "Range in meters within which 'nearby' sharing will activate"
    field :nearby_distance, :integer

    @desc "Cooldown period between 'nearby' sharing notifications"
    field :nearby_cooldown, :integer
  end

  input_object :friend_share_update_input do
    @desc "The ID of the contact user"
    field :user_id, non_null(:uuid)

    @desc "The share mode for the contact user"
    field :share_type, non_null(:friend_share_type)

    @desc "The location share config for the contact user"
    field :share_config, :friend_share_config_input

    @desc "Allows the user to set their location when updating sharing"
    field :location, :user_location_update_input
  end

  payload_object(:friend_update_payload, :friend)

  input_object :friend_delete_input do
    @desc """
    The ID of the user remove as a friend or whose invitation to decline
    or cancel
    """
    field :user_id, non_null(:uuid)
  end

  payload_object(:friend_delete_payload, :boolean)

  @desc "DEPRECATED Parameters for starting a live location share"
  input_object :user_location_live_share_input do
    @desc "The user with whom to share location"
    field :shared_with_id, non_null(:string)

    @desc "The expiry for the share"
    field :expires_at, non_null(:datetime)

    @desc "The user's current location"
    field :location, :user_location_update_input
  end

  payload_object(:user_location_live_share_payload, :user_location_live_share)

  @desc "DEPRECATED Parameters for canceling a live location share"
  input_object :user_location_cancel_share_input do
    @desc "The user whose location sharing to cancel"
    field :shared_with_id, non_null(:string)
  end

  payload_object(:user_location_cancel_share_payload, :boolean)

  object :friend_mutations do
    @desc """
    Invite another user to be your friend or accept an existing invitation from
    them
    """
    field :friend_invite, type: :friend_invite_payload do
      arg :input, non_null(:friend_invite_input)
      resolve &Contact.friend_invite/2
      changeset_mutation_middleware()
    end

    @desc "Update the location sharing settings of a friendship"
    field :friend_share_update, type: :friend_update_payload do
      arg :input, non_null(:friend_share_update_input)
      resolve &Contact.friend_share_update/2
      changeset_mutation_middleware()
    end

    @desc """
    Remove the friendship between the sender and another user or decline
    or cancel an invitation from or to them
    """
    field :friend_delete, type: :friend_delete_payload do
      arg :input, non_null(:friend_delete_input)
      resolve &Contact.friend_delete/2
      changeset_mutation_middleware()
    end

    @desc "DEPRECATED Share the user's location"
    field :user_location_live_share, type: :user_location_live_share_payload do
      deprecate "Set sharing options via friendShareUpdate"
      arg :input, non_null(:user_location_live_share_input)
      resolve &Contact.live_share_location/2
      changeset_mutation_middleware()
    end

    @desc "DEPRECATED Cancel a live location share"
    field :user_location_cancel_share, type: :user_location_cancel_share_payload do
      deprecate "Set sharing options via friendShareUpdate"
      arg :input, non_null(:user_location_cancel_share_input)
      resolve &Contact.cancel_location_share/2
      changeset_mutation_middleware()
    end

    @desc "DEPRECATED Cancel all live location shares"
    field :user_location_cancel_all_shares,
      type: :user_location_cancel_share_payload do
      deprecate "Set sharing options via friendShareUpdate"
      resolve &Contact.cancel_all_location_shares/2
      changeset_mutation_middleware()
    end
  end

  # -------------------------------------------------------------------
  # Subscriptions

  object :friend_subscriptions do
    @desc """
    Receive an update when a contact's state (friended/unfriended) changes
    """
    field :contacts, non_null(:contact_relationship_change) do
      user_subscription_config(&Contact.contacts_subscription_topic/1)
    end

    @desc """
    Receive an update when a friend's data (eg name, handle) changes
    """
    field :friends, non_null(:user) do
      user_subscription_config(&Contact.friends_subscription_topic/1)
    end

    @desc """
    Receive an update when a friend's shared location changes
    """
    field :shared_locations, non_null(:user_location_update) do
      config fn
        _, %{context: %{current_user: user}} ->
          {:ok,
           topic: Contact.location_subscription_topic(user.id),
           catchup: fn -> Contact.location_catchup(user) end}

        _, _ ->
          {:error, "This operation requires an authenticated user"}
      end
    end
  end
end
