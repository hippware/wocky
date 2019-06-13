defmodule Wocky.PhoneNumber do
  @moduledoc """
  Wocky module for phone number manipulation
  """

  alias ExTwilio.Lookup
  alias Faker.Address

  @type t() :: binary()
  @type country_code() :: binary()

  @spec normalise(t(), country_code()) :: {:ok, t()} | {:error, term()}
  def normalise(input_number, country_code) do
    with {:ok, phone_number} <- ExPhoneNumber.parse(input_number, country_code) do
      {:ok, ExPhoneNumber.format(phone_number, :e164)}
    end
  end

  @spec country_code(t()) :: {:ok, country_code()} | {:error, term()}
  def country_code(phone_number) do
    :wocky
    |> Confex.get_env(:country_code_lookup_method)
    |> get_country_code(phone_number)
  end

  @spec bypass?(t()) :: boolean()
  def bypass?(phone_number) do
    if Application.get_env(:wocky, :enable_auth_bypass) do
      prefixes = Application.get_env(:wocky, :auth_bypass_prefixes)
      String.starts_with?(phone_number, prefixes)
    else
      false
    end
  end

  defp get_country_code(method, phone_number) do
    if bypass?(phone_number) do
      do_get_country_code(:hardwire, phone_number)
    else
      do_get_country_code(method, phone_number)
    end
  end

  defp do_get_country_code(:twilio, phone_number) do
    case Lookup.retrieve(phone_number, type: :phoneNumber) do
      {:ok, info} ->
        {:ok, info.country_code}

      {:error, error, code} ->
        {:error, "Twillio error: #{inspect(error)} #{inspect(code)}"}
    end
  end

  defp do_get_country_code(:faker, _phone_number),
    do: {:ok, Address.country_code()}

  defp do_get_country_code(:hardwire, _phone_number),
    do: {:ok, Confex.get_env(:wocky, :country_code_hardwire_value)}
end
