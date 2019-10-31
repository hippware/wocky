defmodule WockyAPI.Resolvers.User do
  @moduledoc "GraphQL resolver for user objects"

  use Elixometer

  alias Wocky.Account
  alias Wocky.Events.LocationRequest
  alias Wocky.Location
  alias Wocky.Location.UserLocation
  alias Wocky.Notifier
  alias Wocky.Notifier.Push

  @default_search_results 50

  # -------------------------------------------------------------------
  # Queries

  def get_first_name(user, _, _), do: {:ok, Account.first_name(user)}

  def get_last_name(user, _, _), do: {:ok, Account.last_name(user)}

  def get_current_user(_root, _args, %{context: %{current_user: user}}) do
    {:ok, user}
  end

  def get_current_user(_root, _args, _info) do
    {:error, "This operation requires an authenticated user"}
  end

  def get_user(_root, %{id: id}, %{context: %{current_user: %{id: id} = u}}) do
    {:ok, u}
  end

  def get_user(_root, %{id: id}, %{context: %{current_user: current_user}}) do
    case Account.get_user(id, current_user) do
      nil -> user_not_found(id)
      user -> {:ok, user}
    end
  end

  def user_not_found(id), do: {:error, "User not found: " <> id}

  def search_users(_root, %{limit: limit}, _info) when limit < 0 do
    {:error, "limit cannot be less than 0"}
  end

  def search_users(_root, %{search_term: search_term} = args, %{
        context: %{current_user: current_user}
      }) do
    limit = args[:limit] || @default_search_results
    {:ok, Account.search_by_name(search_term, current_user, limit)}
  end

  # -------------------------------------------------------------------
  # User mutations

  def update_user(_root, args, %{context: %{current_user: user}}) do
    input = args[:input][:values] |> fix_name(user)

    Account.update(user, input)
  end

  defp fix_name(m, user) do
    new_name = do_fix_name(m, user)

    if new_name do
      Map.put_new(m, :name, String.trim(new_name))
    else
      m
    end
  end

  defp do_fix_name(%{first_name: f, last_name: l}, _user),
    do: f <> " " <> l

  defp do_fix_name(%{first_name: f}, user),
    do: f <> " " <> Account.last_name(user)

  defp do_fix_name(%{last_name: l}, user),
    do: Account.first_name(user) <> " " <> l

  defp do_fix_name(_m, _user), do: nil

  def delete_user(_root, _args, %{context: %{current_user: user}}) do
    Account.delete(user.id)
    {:ok, true}
  end

  # -------------------------------------------------------------------
  # User invitation mutations

  def make_invite_code(_root, _args, %{context: %{current_user: user}}) do
    code = Account.make_invite_code(user)
    {:ok, %{successful: true, result: code}}
  end

  def redeem_invite_code(_root, args, %{context: %{current_user: user}}) do
    result = Account.redeem_invite_code(user, args[:input][:code])
    {:ok, %{successful: result, result: result}}
  end

  # -------------------------------------------------------------------
  # Push notification mutations

  def enable_notifications(%{input: i}, %{context: %{current_user: user}}) do
    platform = Map.get(i, :platform)
    dev_mode = Map.get(i, :dev_mode)

    :ok = Push.enable(user, i.device, i.token, platform, dev_mode)
    {:ok, true}
  end

  def disable_notifications(%{input: i}, %{context: %{current_user: user}}) do
    :ok = Push.disable(user, i.device)
    {:ok, true}
  end

  # -------------------------------------------------------------------
  # User location mutations

  def get_location_token(_root, _args, %{context: %{current_user: user}}) do
    {:ok, token} = Account.get_location_jwt(user)

    {:ok, %{successful: true, result: token}}
  end

  def update_location(_root, %{input: i}, %{context: %{current_user: user}}) do
    location = UserLocation.new(i)

    with {:ok, _} <- Location.set_user_location(user, location) do
      update_counter("foreground_location_uploads", 1)
      {:ok, Location.get_watched_status(user)}
    end
  end

  def trigger_location_request(_root, %{input: %{user_id: user_id}}, _info) do
    if Confex.get_env(:wocky_api, :enable_location_request_trigger) do
      # Trigger the silent push notification
      user = Account.get_user(user_id)

      if user do
        event = %LocationRequest{to: user}
        Notifier.notify(event)

        {:ok, true}
      else
        {:ok, false}
      end
    else
      {:ok, false}
    end
  end
end
