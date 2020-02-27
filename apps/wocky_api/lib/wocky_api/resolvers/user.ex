# credo:disable-for-this-file Credo.Check.Readability.Specs
defmodule WockyAPI.Resolvers.User do
  @moduledoc "GraphQL resolver for user objects"

  use Elixometer

  import WockyAPI.Resolvers.Utils

  alias Wocky.Account
  alias Wocky.Audit
  alias Wocky.Contacts
  alias Wocky.Events.LocationRequest
  alias Wocky.Location
  alias Wocky.Location.UserLocation
  alias Wocky.Notifier
  alias Wocky.Notifier.Push
  alias Wocky.PhoneNumber

  @default_search_results 50
  @max_lookups 100

  # -------------------------------------------------------------------
  # Queries

  def get_first_name(user, _, _), do: {:ok, Account.first_name(user)}

  def get_last_name(user, _, _), do: {:ok, Account.last_name(user)}

  def get_current_user(_args, %{context: %{current_user: user}}) do
    {:ok, user}
  end

  def get_current_user(_args, _info) do
    {:error, "This operation requires an authenticated user"}
  end

  def get_user(%{id: id}, %{context: %{current_user: %{id: id} = u}}) do
    {:ok, u}
  end

  def get_user(%{id: id}, %{context: %{current_user: current_user}}) do
    case Account.get_user(id, current_user) do
      nil -> user_not_found(id)
      user -> {:ok, user}
    end
  end

  def get_users(%{limit: limit}, _info) when limit < 0 do
    {:error, "limit cannot be less than 0"}
  end

  def get_users(%{search_term: search_term} = args, %{
        context: %{current_user: current_user}
      }) do
    limit = args[:limit] || @default_search_results
    {:ok, Account.search_by_name(search_term, current_user, limit)}
  end

  def get_user_bulk_lookup(args, %{context: %{current_user: user}}) do
    numbers = args[:phone_numbers]

    with :ok <- check_lookup_count(numbers),
         {:ok, cc} <- PhoneNumber.country_code(user.phone_number) do
      {prepared_numbers, failed_numbers} =
        numbers
        |> Enum.uniq()
        |> Enum.reduce({%{}, []}, &maybe_prepare_number(&1, cc, &2))

      lookup_results =
        prepared_numbers
        |> Map.keys()
        |> Account.get_by_phone_number(user)
        |> Enum.flat_map_reduce(
          prepared_numbers,
          &build_lookup_result(&1, &2, user)
        )
        |> merge_non_user_results()
        |> merge_failed_results(failed_numbers)

      {:ok, lookup_results}
    end
  end

  defp check_lookup_count(numbers) do
    if length(numbers) <= @max_lookups do
      :ok
    else
      {:error, "Maximum bulk operation size is #{@max_lookups}"}
    end
  end

  defp maybe_prepare_number(number, country_code, {acc, failed}) do
    case PhoneNumber.normalise(number, country_code) do
      {:ok, n} ->
        {Map.update(acc, n, [number], &[number | &1]), failed}

      {:error, _} ->
        {acc, [number | failed]}
    end
  end

  defp build_lookup_result(user, numbers, requestor) do
    request_numbers = Map.get(numbers, user.phone_number)
    relationship = Contacts.relationship(requestor, user)

    {
      Enum.map(
        request_numbers,
        &%{
          phone_number: &1,
          e164_phone_number: user.phone_number,
          user: user,
          relationship: relationship
        }
      ),
      Map.delete(numbers, user.phone_number)
    }
  end

  defp merge_non_user_results({lookup_results, leftover_numbers}) do
    leftover_numbers
    |> Enum.flat_map(&build_non_user_result/1)
    |> Enum.concat(lookup_results)
  end

  defp build_non_user_result({e164_phone_number, numbers}) do
    Enum.map(
      numbers,
      &%{phone_number: &1, e164_phone_number: e164_phone_number}
    )
  end

  defp merge_failed_results(results, failed_numbers) do
    failed_numbers
    |> Enum.map(&%{phone_number: &1})
    |> Enum.concat(results)
  end

  # -------------------------------------------------------------------
  # User mutations

  def user_update(args, %{context: %{current_user: user}}) do
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

  def user_delete(_args, %{context: %{current_user: user}}) do
    Account.delete(user)
    {:ok, true}
  end

  # -------------------------------------------------------------------
  # Push notification mutations

  def push_notifications_enable(%{input: i}, %{context: %{current_user: user}}) do
    platform = Map.get(i, :platform)
    dev_mode = Map.get(i, :dev_mode)

    :ok = Push.enable(user, i.device, i.token, platform, dev_mode)
    {:ok, true}
  end

  def push_notifications_disable(%{input: i}, %{context: %{current_user: user}}) do
    :ok = Push.disable(user, i.device)
    {:ok, true}
  end

  # -------------------------------------------------------------------
  # User location mutations

  def user_location_get_token(_args, %{context: %{current_user: user}}) do
    {:ok, token} = Account.get_location_jwt(user)

    {:ok, %{successful: true, result: token}}
  end

  def user_location_update(%{input: i}, %{context: %{current_user: user}}) do
    location = UserLocation.new(i)

    with {:ok, _} <- Location.set_user_location(user, location) do
      update_counter("foreground_location_uploads", 1)
      {:ok, Location.get_watched_status(user)}
    end
  end

  def user_location_request_trigger(%{input: %{user_id: user_id}}, _info) do
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

  # -------------------------------------------------------------------
  # Debug mutations

  def get_user_full_audit(_, %{context: %{current_user: user}}) do
    {:ok, Audit.user_audit_enabled?(user)}
  end

  def user_full_audit(%{input: %{enable: enable}}, %{
        context: %{current_user: user}
      }) do
    if enable do
      Audit.enable_user_audit(user)
    else
      Audit.disable_user_audit(user)
    end

    {:ok, true}
  end
end
