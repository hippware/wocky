defmodule WockyAPI.Schema.UserInviteTypes do
  @moduledoc """
  Absinthe types for inviting non-users to join the service and become
  friends.
  """

  use WockyAPI.Schema.Notation

  alias WockyAPI.Resolvers.UserInvite

  # -------------------------------------------------------------------
  # Mutations

  payload_object(:user_invite_make_code_payload, :string)

  input_object :user_invite_redeem_code_input do
    @desc "The invite code to redeem"
    field :code, non_null(:string)
  end

  payload_object(:user_invite_redeem_code_payload, :boolean)

  # DEPRECATED
  input_object :friend_bulk_invite_input do
    @desc "A list of phone numbers to which to send invitations"
    field :phone_numbers, non_null(list_of(non_null(:string)))
  end

  # DEPRECATED
  enum :bulk_invite_result do
    @desc "An invitation was sent externally (e.g. SMS)"
    value :external_invitation_sent

    @desc "An invitation was sent internally (i.e. to an existing user)"
    value :internal_invitation_sent

    @desc """
    The requestor and target user are already friends - no action was taken
    """
    value :already_friends

    @desc """
    The requestor and target are the same person - no action was taken
    """
    value :self

    @desc "The supplied number could not be parsed into normalised form"
    value :could_not_parse_number

    @desc "There was an error attempting to send an SMS"
    value :sms_error
  end

  # DEPRECATED
  @desc "Single result for friendBulkInvite"
  object :friend_bulk_invite_result do
    @desc "The original input phone number for which this is the result set"
    field :phone_number, non_null(:string)

    @desc "The E.164 normalised form of the phone number"
    field :e164_phone_number, :string

    @desc "The user, if any, currently associated with the phone number"
    field :user, :user

    @desc "The result of the attempt to send the invitation"
    field :result, non_null(:bulk_invite_result)

    @desc "The details of the error, if any, from sending the invitation"
    field :error, :string
  end

  # DEPRECATED
  payload_object(
    :friend_bulk_invite_payload,
    list_of(:friend_bulk_invite_result)
  )

  object :user_invite_mutations do
    @desc "Generate a user invite code"
    field :user_invite_make_code, type: :user_invite_make_code_payload do
      resolve &UserInvite.user_invite_make_code/2
    end

    @desc "Redeem a user invite code"
    field :user_invite_redeem_code, type: :user_invite_redeem_code_payload do
      arg :input, non_null(:user_invite_redeem_code_input)
      resolve &UserInvite.user_invite_redeem_code/2
    end

    @desc """
    DEPRECATED Send invitations to all the phone numbers supplied. Numbers are
    normalised as per `userBulkLookup`.

    Where a number already has an associated user, and the user is not already
    the requestor's friend or invitee, an in-system invitation will be sent.

    Where a number does not already have a user, an SMS will be sent to invite
    the owner to join the app.
    """
    field :friend_bulk_invite, type: :friend_bulk_invite_payload do
      deprecate "This API will be removed in a future version"
      @desc "The list of phone numbers to invite"
      arg :input, non_null(:friend_bulk_invite_input)
      resolve &UserInvite.friend_bulk_invite/2
      middleware &build_payload/2
      middleware WockyAPI.Middleware.RefreshCurrentUser
    end
  end
end
