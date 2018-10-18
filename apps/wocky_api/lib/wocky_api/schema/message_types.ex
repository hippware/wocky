defmodule WockyAPI.Schema.MessageTypes do
  @moduledoc """
  Absinthe types for inter-user messages
  """

  use WockyAPI.Schema.Notation
  use Absinthe.Ecto, repo: Wocky.Repo

  import Kronky.Payload

  alias WockyAPI.Resolvers.Message

  @desc "A message to a user"
  object :message do
    @desc "The user who sent or received the message"
    field :other_user, non_null(:user), resolve: assoc(:other_user)

    @desc "The message stanza itself"
    field :message, non_null(:string)

    @desc "Direction of the message was sent with respect to the requesting user"
    field :direction, non_null(:message_direction)

    @desc "The time the messages was received by the server"
    field :created_at, non_null(:datetime)
  end

  connection :messages, node_type: :message do
    total_count_field()

    edge do
    end
  end

  connection :conversations, node_type: :message do
    total_count_field()

    edge do
    end
  end

  enum :message_direction do
    @desc "The message was sent to the current user"
    value :incoming

    @desc "The message was sent by the current user"
    value :outgoing
  end

  input_object :send_message_input do
    field :recipient_id, non_null(:uuid)
    field :message, non_null(:string)
  end

  payload_object(:send_message_payload, :boolean)

  object :message_mutations do
    field :send_message, type: :send_message_payload do
      arg :input, non_null(:send_message_input)
      resolve &Message.send_message/3
      changeset_mutation_middleware()
    end
  end

  @desc "Subscribe to incoming messages for the current user"
  object :message_subscriptions do
    field :messages, non_null(:message) do
      user_subscription_config(&Message.messages_subscription_topic/1)
    end
  end
end
