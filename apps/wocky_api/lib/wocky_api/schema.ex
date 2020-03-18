defmodule WockyAPI.Schema do
  @moduledoc "GraphQL schema for the Wocky API"

  @behaviour Absinthe.Schema

  use Absinthe.Schema
  use Absinthe.Relay.Schema, :modern

  alias WockyAPI.Dataloader
  alias WockyAPI.Middleware.Auth
  alias WockyAPI.Middleware.QueryCounter
  alias WockyAPI.Middleware.QueryTimer

  import_types Absinthe.Type.Custom
  import_types AbsintheErrorPayload.ValidationMessageTypes
  import_types WockyAPI.Schema.AuthTypes
  import_types WockyAPI.Schema.BlockTypes
  import_types WockyAPI.Schema.BotTypes
  import_types WockyAPI.Schema.ContactTypes
  import_types WockyAPI.Schema.MediaTypes
  import_types WockyAPI.Schema.MessageTypes
  import_types WockyAPI.Schema.MetadataTypes
  import_types WockyAPI.Schema.NotificationTypes
  import_types WockyAPI.Schema.PresenceTypes
  import_types WockyAPI.Schema.TestingTypes
  import_types WockyAPI.Schema.UserInviteTypes
  import_types WockyAPI.Schema.UserTypes
  import_types WockyAPI.Types.AInt
  import_types WockyAPI.Types.UUID

  query do
    import_fields :bot_queries
    import_fields :media_queries
    import_fields :metadata_queries
    import_fields :notification_queries
    import_fields :user_invite_queries
    import_fields :user_queries
  end

  mutation do
    import_fields :auth_mutations
    import_fields :block_mutations
    import_fields :bot_mutations
    import_fields :debug_mutations
    import_fields :friend_mutations
    import_fields :location_mutations
    import_fields :media_mutations
    import_fields :message_mutations
    import_fields :notification_mutations
    import_fields :presence_mutations
    import_fields :push_notifications_mutations
    import_fields :testing_mutations
    import_fields :user_invite_mutations
    import_fields :user_mutations
  end

  subscription do
    import_fields :bot_subscriptions
    import_fields :friend_subscriptions
    import_fields :message_subscriptions
    import_fields :notification_subscriptions
    import_fields :presence_subscriptions
  end

  @impl true
  def middleware(middleware, field, object) do
    [
      ApolloTracing.Middleware.Tracing
      | middleware
        |> Auth.middleware(field, object)
        |> QueryTimer.instrument(field, object)
        |> QueryCounter.instrument(field, object)
    ]
  end

  @impl true
  def plugins, do: [Absinthe.Middleware.Dataloader | Absinthe.Plugin.defaults()]

  @impl true
  def context(ctx), do: Map.put(ctx, :loader, Dataloader.get(ctx))
end
