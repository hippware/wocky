defmodule WockyAPI.Resolvers.BulkUser do
  @moduledoc "GraphQL resolver for bulk user operations"

  alias Wocky.{PhoneNumber, Roster, User}
  alias Wocky.Roster.BulkInvitation

  @max_lookups 100

  def lookup(_root, args, %{context: %{current_user: user}}) do
    numbers = args[:phone_numbers]

    with :ok <- check_lookup_count(numbers),
         {:ok, cc} <- PhoneNumber.country_code(user.phone_number) do
      unique_numbers = Enum.uniq(numbers)

      prepared_numbers =
        Enum.reduce(unique_numbers, %{}, &maybe_prepare_number(&1, cc, &2))

      lookup_results =
        prepared_numbers
        |> Map.keys()
        |> User.get_by_phone_number(user)
        |> Enum.reduce(prepared_numbers, &merge_lookup_result(&1, &2, user))
        |> Enum.map(&normalise_result/1)
        |> add_failed_results(unique_numbers)

      {:ok, lookup_results}
    end
  end

  def send_invitations(_root, args, %{context: %{current_user: user}}) do
    {:ok, BulkInvitation.send(args[:input][:phone_numbers], user)}
  end

  defp check_lookup_count(numbers) do
    if length(numbers) <= @max_lookups do
      :ok
    else
      {:error, "Maximum bulk operation size is #{@max_lookups}"}
    end
  end

  defp maybe_prepare_number(number, country_code, acc) do
    case PhoneNumber.normalise(number, country_code) do
      {:ok, n} -> acc |> Map.put(n, %{phone_number: number})
      {:error, _} -> acc
    end
  end

  defp merge_lookup_result(
         %User{phone_number: phone_number} = user,
         prepared_numbers,
         requestor
       ) do
    Map.put(
      prepared_numbers,
      phone_number,
      prepared_numbers
      |> Map.get(phone_number)
      |> Map.put(:user, user)
      |> Map.put(:relationship, Roster.relationship(requestor, user))
    )
  end

  defp normalise_result({normalised_number, data}) do
    %{
      phone_number: data[:phone_number],
      e164_phone_number: normalised_number,
      user: data[:user],
      relationship: data[:relationship]
    }
  end

  defp add_failed_results(lookup_results, input_numbers) do
    successful_numbers = Enum.map(lookup_results, & &1.phone_number)
    failed_numbers = input_numbers -- successful_numbers

    failed_numbers
    |> Enum.map(&%{phone_number: &1})
    |> Enum.concat(lookup_results)
  end
end
