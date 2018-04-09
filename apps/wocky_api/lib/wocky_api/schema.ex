defmodule WockyAPI.Schema do
  @moduledoc "GraphQL schema for the Wocky API"

  use Absinthe.Schema
  use Absinthe.Relay.Schema, :modern

  import_types Absinthe.Type.Custom
  import_types Kronky.ValidationMessageTypes
  import_types WockyAPI.Schema.AuthTypes
  import_types WockyAPI.Schema.BotTypes
  import_types WockyAPI.Schema.UserTypes
  import_types WockyAPI.Types.UUID

  query do
    import_fields :user_queries
    import_fields :bot_queries
  end

  mutation do
    import_fields :auth_mutations
    import_fields :user_mutations
    import_fields :bot_mutations
    import_fields :location_mutations
  end

  subscription do
    import_fields :bot_subscriptions
  end

  # Fields available only to the authenticated user on themself
  def middleware(middleware, %{identifier: field}, %{identifier: :user})
  when field == :external_id
    or field == :phone_number
    or field == :email do
    [WockyAPI.Middleware.AuthSelf | middleware]
  end

  # Data publicly available
  def middleware(middleware, %{identifier: field}, %{identifier: :user})
  when field == :id
    or field == :server
    or field == :handle
    or field == :avatar
    or field == :tagline
    or field == :roles
    or field == :bots do
    middleware
  end
  def middleware(middleware, _field, %{identifier: object})
  when object == :query
    or object == :bot
    or object == :bot_item
    or object == :subscribers_connection
    or object == :subscribers_edge
    or object == :bots_connection
    or object == :bots_edge do
    # We filter by the public flag on bot in the resolver
    middleware
  end
  def middleware(middleware, %{identifier: :authenticate}, %{identifier: :mutation}) do
    middleware
  end

  # Data requireing standard authentication
  def middleware(middleware, _field, _object) do
    [WockyAPI.Middleware.Auth | middleware]
  end

end
