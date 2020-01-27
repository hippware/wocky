defmodule WockyAPI.GraphQLHelper do
  @moduledoc """
  Helper functions for Wocky API GraphQL tests
  """

  alias Wocky.Account.User
  alias Wocky.GeoUtils
  alias Wocky.POI.Bot
  alias Wocky.Repo.Timestamp

  @spec run_query(binary(), User.t() | nil, map()) ::
          Absinthe.result_t() | no_return()
  def run_query(query, user \\ nil, variables \\ %{}) do
    Absinthe.run!(
      query,
      WockyAPI.Schema,
      variables: variables,
      context: build_context(user)
    )
  end

  defp build_context(%User{} = user), do: %{current_user: user}
  defp build_context(_), do: %{}

  @spec has_data(map()) :: boolean()
  def has_data(result) do
    Map.has_key?(result, :data)
  end

  @spec has_errors(map()) :: boolean()
  def has_errors(result) do
    Map.has_key?(result, :errors)
  end

  @spec error_count(map()) :: non_neg_integer()
  def error_count(result) do
    result |> Map.get(:errors, []) |> length()
  end

  @spec error_msg(map(), non_neg_integer()) :: String.t()
  def error_msg(result, idx \\ 0) do
    error =
      result
      |> Map.get(:errors, [])
      |> Enum.at(idx, %{})

    error[:message]
  end

  @spec successful?(map()) :: boolean()
  def successful?(%{"successful" => successful}), do: successful

  @spec failure_msg(map()) :: String.t()
  def failure_msg(%{"messages" => [%{"message" => message} | _]}),
    do: message

  @spec stringify_keys(map()) :: map()
  def stringify_keys(map) do
    Enum.into(map, %{}, fn {k, v} -> {to_string(k), v} end)
  end

  # credo:disable-for-next-line Credo.Check.Readability.Specs
  def bot_create_fields do
    [
      :title,
      :server,
      :lat,
      :lon,
      :radius,
      :description,
      :shortname,
      :geofence
    ]
  end

  # credo:disable-for-next-line Credo.Check.Readability.Specs
  def add_bot_lat_lon(%Bot{location: location} = bot) do
    {lat, lon} = GeoUtils.get_lat_lon(location)
    bot |> Map.put(:lat, lat) |> Map.put(:lon, lon)
  end

  @spec sharing_expiry(non_neg_integer()) :: String.t()
  def sharing_expiry(days \\ 5) do
    Timestamp.shift(days: days)
    |> DateTime.truncate(:second)
    |> Timestamp.to_string!()
  end
end
