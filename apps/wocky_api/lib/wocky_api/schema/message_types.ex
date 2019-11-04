defmodule WockyAPI.Schema.MessageTypes do
  @moduledoc """
  Absinthe types for inter-user messages
  """

  use WockyAPI.Schema.Notation

  alias WockyAPI.Resolvers.Media
  alias WockyAPI.Resolvers.Message

  # -------------------------------------------------------------------
  # Objects

  enum :message_direction do
    @desc "The message was sent to the current user"
    value :incoming

    @desc "The message was sent by the current user"
    value :outgoing
  end

  @desc "A message to a user"
  object :message do
    @desc "The ID of this message"
    field :id, non_null(:integer)

    @desc "The user who sent or received the message"
    field :other_user, non_null(:user)

    @desc "The message content"
    field :content, :string

    @desc "Media contained in the media"
    field :media, :media, do: resolve(&Media.get_media/3)

    @desc "Direction of the message was sent with respect to the requesting user"
    field :direction, non_null(:message_direction)

    @desc "Whether the recipient has marked the message as read"
    field :read, non_null(:boolean)

    @desc "Client-supplied metadata - opaque to server"
    field :client_data, :string

    @desc "The time the messages was received by the server"
    field :created_at, non_null(:datetime)

    @desc "The time the messages was last updated"
    field :updated_at, non_null(:datetime)
  end

  # -------------------------------------------------------------------
  # Connections

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

  # -------------------------------------------------------------------
  # Mutations

  @desc "DEPRECATED - use messageSendInput"
  input_object :send_message_input do
    @desc "Message recipient"
    field :recipient_id, non_null(:uuid)

    @desc "Textual content of the message"
    field :content, :string

    @desc "TROS URL of any attached media"
    field :image_url, :string
  end

  input_object :message_send_input do
    @desc "Message recipient"
    field :recipient_id, non_null(:uuid)

    @desc "Textual content of the message"
    field :content, :string

    @desc "TROS URL of any attached media"
    field :image_url, :string

    @desc "Client data - opaque to server"
    field :client_data, :string
  end

  payload_object(:message_send_payload, :boolean)

  input_object(:message_mark_read) do
    @desc "ID of message to mark"
    field :id, non_null(:integer)

    @desc "Whether the message is marked as read or unread (default: true)"
    field :read, :boolean
  end

  input_object :message_mark_read_input do
    @desc "List of messages to mark as read"
    field :messages, non_null(list_of(:message_mark_read))
  end

  object :message_mark_read_result do
    @desc "ID of message being marked"
    field :id, non_null(:integer)

    @desc "Result of marking"
    field :successful, non_null(:boolean)

    @desc "Any error that occurred"
    field :error, :string
  end

  payload_object(:message_mark_read_payload, list_of(:message_mark_read_result))

  object :message_mutations do
    @desc "Send a message to another user"
    field :message_send, type: :message_send_payload do
      arg :input, non_null(:message_send_input)
      resolve &Message.send_message/3
      changeset_mutation_middleware()
    end

    @desc "Mark a message's read status"
    field :message_mark_read, type: :message_mark_read_payload do
      arg :input, non_null(:message_mark_read_input)
      resolve &Message.mark_read/3
      changeset_mutation_middleware()
    end
  end

  # -------------------------------------------------------------------
  # Subscriptions

  @desc "Subscribe to incoming messages for the current user"
  object :message_subscriptions do
    field :messages, non_null(:message) do
      user_subscription_config(&Message.messages_subscription_topic/1)
    end
  end
end
