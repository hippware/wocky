defmodule WockyAPI.Schema.BulkUserTypes do
  @moduledoc """
  Absinthe types for wocky bulk invitations
  """

  use WockyAPI.Schema.Notation

  import Kronky.Payload

  alias WockyAPI.Resolvers.BulkUser

  @desc "Single result for userBulkLookup"
  object :user_bulk_lookup_result do
    @desc "The original input phone number for which this is the result set"
    field :phone_number, non_null(:string)

    @desc "The E.164 normalised form of the phone number"
    field :e164_phone_number, :string

    @desc "The user, if any, currently associated with the phone number"
    field :user, :user

    @desc "The relationship of the requestor to the returned user"
    field :relationship, :user_contact_relationship
  end

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

  input_object :friend_bulk_invite_input do
    @desc "A list of phone numbers to which to send invitations"
    field :phone_numbers, non_null(list_of(non_null(:string)))
  end

  payload_object(
    :friend_bulk_invite_payload,
    list_of(:friend_bulk_invite_result)
  )

  object :bulk_user_queries do
    @desc """
    Lookup users by phone number. Numbers not already in E.164 format will be
    attempted to be parsed and normalised based on the user's country (which
    in turn is inferred from their phone number as received from Firebase).

    Bypass numbers will be treated as being in the US.
    """
    field :user_bulk_lookup, type: list_of(:user_bulk_lookup_result) do
      @desc "The list of phone numbers to lookup"
      arg :phone_numbers, non_null(list_of(non_null(:string)))
      resolve &BulkUser.lookup/3
    end
  end

  object :bulk_user_mutations do
    @desc """
    Send invitations to all the phone numbers supplied. Numbers are normalised
    as per userBulkLookup.

    Where a number already has an associated user, and the user is not already
    the requestor's friend or invitee, an in-system invitation will be sent.

    Where a number does not already have a user, an SMS will be sent to invite
    the owner to join the app.
    """
    field :friend_bulk_invite, type: :friend_bulk_invite_payload do
      @desc "The list of phone numbers to invite"
      arg :input, non_null(:friend_bulk_invite_input)
      resolve &BulkUser.send_invitations/3
      middleware &build_payload/2
      middleware WockyAPI.Middleware.RefreshCurrentUser
    end
  end
end
