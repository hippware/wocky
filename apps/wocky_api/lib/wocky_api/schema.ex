defmodule WockyAPI.Schema do
  @moduledoc "GraphQL schema for the Wocky API"

  use Absinthe.Schema
  use Absinthe.Relay.Schema, :modern

  import_types Kronky.ValidationMessageTypes
  import_types WockyAPI.Schema.BotTypes
  import_types WockyAPI.Schema.UserTypes
  import_types WockyAPI.Type.UUID

  query do
    import_fields :user_queries
    import_fields :bot_queries
  end

  mutation do
    import_fields :user_mutations
    import_fields :bot_mutations
    import_fields :location_mutations
  end

  subscription do
    import_fields :bot_subscriptions
  end

  # For now all requests must be authenticated
  # In the future this is where we can distinguish public/private data
  def middleware(middleware, %{identifier: field}, %{identifier: :user})
  when field == :external_id
    or field == :phone_number
    or field == :email do
    [WockyAPI.Middleware.AuthSelf| middleware]
  end

  def middleware(middleware, _field, _object) do
    [WockyAPI.Middleware.Auth | middleware]
  end

end
