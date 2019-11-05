defmodule WockyAPI.Resolvers.BulkUser do
  @moduledoc "GraphQL resolver for bulk user operations"

  alias Wocky.Account
  alias Wocky.Friends
  alias Wocky.Friends.BulkInvitation
  alias Wocky.PhoneNumber

  @max_lookups 100

  # -------------------------------------------------------------------
  # Queries - bulk user lookup

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
    relationship = Friends.relationship(requestor, user)

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
  # Mutations - bulk friend invite

  def friend_bulk_invite(args, %{context: %{current_user: user}}) do
    {:ok, BulkInvitation.send(args[:input][:phone_numbers], user)}
  end
end
