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

  defp get_country_code(:twilio, phone_number) do
    with {:ok, info} <- Lookup.retrieve(phone_number, type: :phoneNumber) do
      {:ok, info.country_code}
    end
  end

  defp get_country_code(:faker, _phone_number),
    do: {:ok, Address.country_code()}

  defp get_country_code({:hardwire, code}, _phone_number), do: {:ok, code}
end
