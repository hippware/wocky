defmodule Wocky.Account.JWT.VerifyTest do
  use ExUnit.Case

  alias Wocky.Account.JWT.Client, as: ClientJWT
  alias Wocky.Account.JWT.Verify

  defp verify_claim(name, value) do
    claims = %{name => value}
    {:ok, claims} == Verify.verify_claim(ClientJWT, name, claims, [])
  end

  test "iat validation" do
    assert verify_claim("iat", nil)
    assert verify_claim("iat", Guardian.timestamp())
    refute verify_claim("iat", Guardian.timestamp() + 100)
  end
end
