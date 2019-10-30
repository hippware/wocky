defmodule WockyAPI.Schema.FriendTypes do
  @moduledoc """
  Absinthe types for user friends
  """

  use WockyAPI.Schema.Notation

  alias WockyAPI.Resolvers.Friend

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

  @desc "Another user with whom a relationship exists"
  object :contact do
    @desc "The other user"
    field :user, non_null(:user), resolve: &Friend.get_contact_user/3

    @desc "The current user's relationship with the other user"
    field :relationship, :user_contact_relationship

    @desc "The current user's nickname for the other user"
    field :name, :string

    @desc "The creation time of the contact"
    field :created_at, non_null(:datetime)
  end

  @desc "An invitation from one user to another to become a friend"
  object :invitation do
    @desc "The sender"
    field :sender, :user, resolve: dataloader(Wocky, :user)

    @desc "The recipient"
    field :recipient, :user, resolve: dataloader(Wocky, :invitee)

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

  @desc "Attributes of a user location live sharing session"
  object :user_location_live_share do
    @desc "ID for this sharing session"
    field :id, non_null(:aint)

    @desc "The user sharing their location"
    field :user, non_null(:other_user)

    @desc "The user with whom the location is being shared"
    field :shared_with, non_null(:other_user)

    @desc "When the share was created"
    field :created_at, non_null(:datetime)

    @desc "The expiry for the share"
    field :expires_at, non_null(:datetime)
  end

  # -------------------------------------------------------------------
  # Connections

  connection :contacts, node_type: :user do
    total_count_field()

    edge do
      @desc "The relationship between the parent and child users"
      field :relationship, :user_contact_relationship,
        do: resolve(&Friend.get_contact_relationship/3)

      @desc "When the relationship was created"
      field :created_at, non_null(:datetime),
        do: resolve(&Friend.get_contact_created_at/3)
    end
  end

  connection :friends, node_type: :contact do
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
  end

  payload_object(:friend_invite_payload, :user_contact_relationship)

  input_object :friend_name_input do
    @desc "The ID of the user to whom to assign a name"
    field :user_id, non_null(:uuid)

    @desc "The name to assign to the specified user"
    field :name, non_null(:string)
  end

  payload_object(:friend_name_payload, :boolean)

  input_object :friend_delete_input do
    @desc """
    The ID of the user remove as a friend or whose invitation to decline
    or cancel
    """
    field :user_id, non_null(:uuid)
  end

  payload_object(:friend_delete_payload, :boolean)

  @desc "Parameters for starting a live location share"
  input_object :user_location_live_share_input do
    @desc "The user with whom to share location"
    field :shared_with_id, non_null(:string)

    @desc "The expiry for the share"
    field :expires_at, non_null(:datetime)

    @desc "The user's current location"
    field :location, :user_location_update_input
  end

  payload_object(:user_location_live_share_payload, :user_location_live_share)

  @desc "Parameters for canceling a live location share"
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
      resolve &Friend.invite/3
      changeset_mutation_middleware()
    end

    @desc "Sets the nickname for a friend"
    # TODO Shouldn't this be :friend_name_update?
    field :friend_name, type: :friend_name_payload do
      arg :input, non_null(:friend_name_input)
      resolve &Friend.name_friend/3
      changeset_mutation_middleware()
    end

    @desc """
    Remove the friendship between the sender and another user or decline
    or cancel an invitation from or to them
    """
    field :friend_delete, type: :friend_delete_payload do
      arg :input, non_null(:friend_delete_input)
      resolve &Friend.unfriend/3
      changeset_mutation_middleware()
    end

    @desc "Share the user's location"
    field :user_location_live_share, type: :user_location_live_share_payload do
      arg :input, non_null(:user_location_live_share_input)
      resolve &Friend.live_share_location/3
      changeset_mutation_middleware()
    end

    @desc "Cancel a live location share"
    field :user_location_cancel_share, type: :user_location_cancel_share_payload do
      arg :input, non_null(:user_location_cancel_share_input)
      resolve &Friend.cancel_location_share/3
      changeset_mutation_middleware()
    end

    @desc "Cancel all live location shares"
    field :user_location_cancel_all_shares,
      type: :user_location_cancel_share_payload do
      resolve &Friend.cancel_all_location_shares/3
      changeset_mutation_middleware()
    end
  end

  # -------------------------------------------------------------------
  # Subscriptions

  @desc "Data that is sent when a user's shared location changes"
  object :user_location_update do
    @desc "The user whose location has changed"
    field :user, non_null(:user)

    @desc "The user's new location"
    field :location, non_null(:location)
  end

  object :friend_subscriptions do
    @desc """
    Receive an update when a contact's state (friended/unfriended) changes
    """
    field :contacts, non_null(:contact) do
      user_subscription_config(&Friend.contacts_subscription_topic/1)
    end

    @desc """
    Receive an update when a friend's data (eg name, handle) changes
    """
    field :friends, non_null(:user) do
      user_subscription_config(&Friend.friends_subscription_topic/1)
    end

    @desc """
    Receive an update when a friend's shared location changes
    """
    field :shared_locations, non_null(:user_location_update) do
      config fn
        _, %{context: %{current_user: user}} ->
          {:ok,
           topic: Friend.location_subscription_topic(user.id),
           catchup: fn -> Friend.location_catchup(user) end}

        _, _ ->
          {:error, "This operation requires an authenticated user"}
      end
    end
  end
end
