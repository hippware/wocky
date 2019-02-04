defmodule WockyAPI.Schema.BulkUserTypes do
  @moduledoc """
  Absinthe types for wocky bulk invitations
  """

  use WockyAPI.Schema.Notation

  import Kronky.Payload

  alias WockyAPI.Resolvers.BulkUser

  object :user_bulk_lookup_result do
    field :phone_number, non_null(:string)
    field :e164_phone_number, :string
    field :user, :user

    @desc "The relationship of the requestor to the returned user"
    field :relationship, :user_contact_relationship
  end

  object :friend_bulk_invite_result do
    field :phone_number, non_null(:string)
    field :e164_phone_number, :string
    field :user, :user
    field :result, non_null(:bulk_invite_result)
    field :error, :string
  end

  enum :bulk_invite_result do
    value :external_invitation_sent
    value :internal_invitation_sent
    value :already_friends
    value :could_not_parse_number
    value :sms_error
  end

  input_object :friend_bulk_invite_input do
    field :phone_numbers, non_null(list_of(non_null(:string)))
  end

  payload_object(
    :friend_bulk_invite_payload,
    list_of(:friend_bulk_invite_result)
  )

  object :bulk_user_queries do
    field :user_bulk_lookup, type: list_of(:user_bulk_lookup_result) do
      arg :phone_numbers, non_null(list_of(non_null(:string)))
      resolve &BulkUser.lookup/3
    end
  end

  object :bulk_user_mutations do
    field :friend_bulk_invite, type: :friend_bulk_invite_payload do
      arg :input, non_null(:friend_bulk_invite_input)
      resolve &BulkUser.send_invitations/3
      middleware &build_payload/2
      middleware WockyAPI.Middleware.RefreshCurrentUser
    end
  end
end
