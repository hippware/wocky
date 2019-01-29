defmodule Wocky.PhoneNumberTest do
  use Wocky.DataCase, async: true

  alias Faker.Phone.EnUs
  alias Wocky.PhoneNumber

  setup do
    orig_lm = Confex.get_env(:wocky, :country_code_lookup_method)
    orig_cc = Confex.get_env(:wocky, :country_code_hardwire_value)
    Application.put_env(:wocky, :country_code_lookup_method, :faker)
    Application.put_env(:wocky, :country_code_hardwire_value, "XX")

    on_exit(fn ->
      Application.put_env(:wocky, :country_code_lookup_method, orig_lm)
      Application.put_env(:wocky, :country_code_hardwire_value, orig_cc)
    end)

    :ok
  end

  test "country_code should always be hardwired value for bypass numbers" do
    prefix = hd(Confex.get_env(:wocky, :auth_bypass_prefixes))
    n = prefix <> EnUs.area_code() <> EnUs.extension()
    assert PhoneNumber.bypass?(n)
    assert {:ok, "XX"} == PhoneNumber.country_code(n)
  end

  test "country_code should not be hardwired value for non-bypass numbers" do
    n = EnUs.area_code() <> EnUs.extension()
    refute PhoneNumber.bypass?(n)
    assert {:ok, cc} = PhoneNumber.country_code(n)
    refute cc == "XX"
  end

  test "any valid number should be able to be normalised" do
    Enum.each(0..100, fn _ ->
      assert {:ok, x} = PhoneNumber.normalise(EnUs.phone(), "US")
      assert x =~ ~r|^\+1\d{10}$|
    end)
  end
end
