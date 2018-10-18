defmodule WockyAPI.Resolvers.Message do
  @moduledoc "GraphQL resolver for message objects"

  alias Absinthe.Subscription
  alias Wocky.{Conversation, Message, Repo, User}
  alias WockyAPI.Endpoint
  alias WockyAPI.Resolvers.User, as: UserResolver
  alias WockyAPI.Resolvers.Utils

  def messages_subscription_topic(user_id),
    do: "messages_subscription_" <> user_id

  def notify_message(message) do
    topic = messages_subscription_topic(message.recipient_id)

    Subscription.publish(
      Endpoint,
      map_to_graphql(message, message.recipient_id),
      [{:messages, topic}]
    )
  end

  def get_messages(_root, args, %{context: %{current_user: user}}) do
    with {:ok, query} <- get_messages_query(args[:other_user], user) do
      query
      |> Utils.connection_from_query(
        user,
        [desc: :created_at],
        &map_to_graphql(&1, user.id),
        args
      )
    end
  end

  defp get_messages_query(nil, requestor),
    do: {:ok, Message.get_query(requestor)}

  defp get_messages_query(other_user_id, requestor) do
    with %User{} = other_user <- User.get_user(other_user_id, requestor) do
      {:ok, Message.get_query(requestor, other_user)}
    else
      _ -> UserResolver.user_not_found(other_user_id)
    end
  end

  def send_message(_root, args, %{context: %{current_user: user}}) do
    recipient_id = args[:input][:recipient_id]

    with %User{} = recipient <- User.get_user(recipient_id, user),
         {:ok, _} <- Message.send(args[:input][:message], recipient, user) do
      {:ok, true}
    else
      _ -> UserResolver.user_not_found(recipient_id)
    end
  end

  def get_conversations(user, args, _info) do
    user.id
    |> Conversation.by_user_query()
    |> Utils.connection_from_query(user, [desc: :created_at], args)
  end

  defp map_to_graphql(%Message{} = message, requestor_id) do
    message = Repo.preload(message, [:sender, :recipient])

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

    data
    |> Map.put(:message, message.message)
    |> Map.put(:created_at, message.created_at)
  end
end
