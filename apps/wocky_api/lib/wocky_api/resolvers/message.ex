# credo:disable-for-this-file Credo.Check.Readability.Specs
defmodule WockyAPI.Resolvers.Message do
  @moduledoc "GraphQL resolver for message objects"

  import WockyAPI.Resolvers.Utils

  alias Wocky.Account
  alias Wocky.Account.User
  alias Wocky.Messaging
  alias Wocky.Messaging.Message

  # -------------------------------------------------------------------
  # Connections

  def get_messages(_root, args, %{context: %{current_user: user}}) do
    with {:ok, query} <- get_messages_query(args[:other_user], user) do
      connection_from_query(
        query,
        user,
        args,
        order_by: [desc: :created_at],
        postprocess: &map_to_graphql(&1, user.id)
      )
    end
  end

  defp get_messages_query(nil, requestor),
    do: {:ok, Messaging.get_messages_query(requestor)}

  defp get_messages_query(other_user_id, requestor) do
    case Account.get_user(other_user_id, requestor) do
      nil ->
        user_not_found(other_user_id)

      other_user ->
        {:ok, Messaging.get_messages_query(requestor, other_user)}
    end
  end

  defp map_to_graphql(%Message{} = message, requestor_id) do
    data =
      if message.sender.id == requestor_id do
        %{
          direction: :outgoing,
          other_user: message.recipient
        }
      else
        %{
          direction: :incoming,
          other_user: message.sender
        }
      end

    message |> Map.merge(data)
  end

  def get_conversations(user, args, _info) do
    user
    |> Messaging.get_conversations_query()
    |> connection_from_query(user, args, order_by: [desc: :created_at])
  end

  # -------------------------------------------------------------------
  # Mutations

  def message_send(%{input: args}, %{context: %{current_user: user}}) do
    recipient_id = args[:recipient_id]

    with %User{} = recipient <- Account.get_user(recipient_id, user),
         {:ok, _} <-
           Messaging.send_message(
             recipient,
             user,
             args[:content],
             args[:image_url],
             args[:client_data]
           ) do
      {:ok, true}
    else
      nil -> user_not_found(recipient_id)
      {:error, :permission_denied} -> {:error, "Permission denied"}
      error -> error
    end
  end

  def message_mark_read(%{input: args}, %{context: %{current_user: user}}) do
    results = Enum.map(args[:messages], &mark_read(&1, user))

    {:ok, results}
  end

  defp mark_read(request, user) do
    read =
      case request[:read] do
        nil -> true
        r -> r
      end

    id = request[:id]

    case Messaging.mark_read(id, user, read) do
      :ok ->
        %{id: id, successful: true}

      {:error, :invalid_id} ->
        %{id: id, successful: false, error: "Invalid message ID"}
    end
  end

  # -------------------------------------------------------------------
  # Subscriptions

  def messages_subscription_topic(user_id),
    do: "messages_subscription_" <> user_id

  def notify_message(message) do
    data = map_to_graphql(message, message.recipient_id)

    message.recipient_id
    |> messages_subscription_topic()
    |> publish_subscription(:messages, data)
  end
end
