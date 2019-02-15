defmodule Wocky.Roster.BulkInvitation do
  @moduledoc """
  Module for sending bulk invitations, both SMS (external) and internal
  """

  alias Wocky.{DynamicLink, PhoneNumber, Roster, User}
  alias Wocky.SMS.Messenger

  @type results :: [map()]

  @spec send([PhoneNumber.t()], User.t()) :: results()
  def send(numbers, user) do
    with {:ok, cc} <- PhoneNumber.country_code(user.phone_number) do
      numbers
      |> Enum.uniq()
      |> Enum.map(&normalise_number(&1, cc))
      |> lookup_users(user)
      |> Enum.map_reduce(%{}, &maybe_send_invitation(&1, &2, user))
      |> elem(0)
    end
  end

  defp normalise_number(number, country_code) do
    case PhoneNumber.normalise(number, country_code) do
      {:ok, norm} ->
        %{phone_number: number, e164_phone_number: norm}

      {:error, e} ->
        %{
          phone_number: number,
          e164_phone_number: nil,
          result: :could_not_parse_number,
          error: inspect(e)
        }
    end
  end

  defp lookup_users(data, requestor) do
    data
    |> Enum.filter(&(&1.e164_phone_number != nil))
    |> Enum.map(& &1.e164_phone_number)
    |> User.get_by_phone_number(requestor)
    |> Enum.reduce(data, &insert_user/2)
  end

  defp insert_user(user, data),
    do: Enum.map(data, &maybe_add_user_to_record(user, &1))

  defp maybe_add_user_to_record(
         %{phone_number: n} = user,
         %{e164_phone_number: n} = r
       ),
       do: Map.put(r, :user, user)

  defp maybe_add_user_to_record(_, r), do: r

  defp maybe_send_invitation(
         %{e164_phone_number: nil} = r,
         sent_numbers,
         _requestor
       ),
       do: {r, sent_numbers}

  defp maybe_send_invitation(
         %{e164_phone_number: number} = r,
         sent_numbers,
         requestor
       ) do
    case sent_numbers[number] do
      nil ->
        send_invitation(r, sent_numbers, requestor)

      {result, error} ->
        r =
          r
          |> Map.put(:result, result)
          |> Map.put(:error, error)

        {r, sent_numbers}
    end
  end

  # We found an unblocked user for this number - send an invite
  defp send_invitation(%{user: user} = r, sent_numbers, requestor) do
    result =
      case Roster.invite(requestor, user) do
        :invited -> :internal_invitation_sent
        :friend -> :already_friends
      end

    {Map.put(r, :result, result),
     Map.put(sent_numbers, r.e164_phone_number, {result, nil})}
  end

  # We didn't find a user for this number - fire an SMS invitation
  defp send_invitation(
         %{e164_phone_number: number} = r,
         sent_numbers,
         requestor
       )
       when not is_nil(number) do
    r =
      with {:ok, body} <- sms_invitation_body(requestor),
           :ok <- Messenger.send(number, body, requestor) do
        Map.put(r, :result, :external_invitation_sent)
      else
        {:error, e} ->
          r
          |> Map.put(:result, :sms_error)
          |> Map.put(:error, inspect(e))

        {:error, e, code} ->
          r
          |> Map.put(:result, :sms_error)
          |> Map.put(:error, "#{inspect(e)}, #{inspect(code)}")
      end

    {r, Map.put(sent_numbers, number, {r[:result], r[:error]})}
  end

  defp sms_invitation_body(user) do
    with {:ok, link} <- DynamicLink.invitation_link(user) do
      {:ok,
       "@#{user.handle} " <>
         maybe_name(user) <>
         "has invited you to tinyrobot." <> " Please visit #{link} to join."}
    end
  end

  defp maybe_name(user) do
    case String.trim("#{user.first_name} #{user.last_name}") do
      "" -> ""
      name -> "(#{name}) "
    end
  end
end
