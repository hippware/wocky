defmodule Wocky.Account.ClientJWT.VerifyTest do
  use ExUnit.Case

  alias Wocky.Account.ClientJWT
  alias Wocky.Account.ClientJWT.Verify

  defp verify_claim(name, value) do
    claims = %{name => value}
    {:ok, claims} == Verify.verify_claim(ClientJWT, name, claims, [])
  end

  test "iss validation" do
    assert verify_claim("iss", "TinyRobot/1.0.0")
    assert verify_claim("iss", "TinyRobot/1.0.0 (Testing)")
    assert verify_claim("iss", "TinyRobot/1.0.0 (Wocky; Testing)")
    refute verify_claim("iss", "Foo")
    refute verify_claim("iss", nil)
  end

  test "aud validation" do
    assert verify_claim("aud", nil)
    assert verify_claim("aud", "Wocky")
    refute verify_claim("aud", "Anything Else")
  end

  test "iat validation" do
    assert verify_claim("iat", nil)
    assert verify_claim("iat", Guardian.timestamp())
    refute verify_claim("iat", Guardian.timestamp() + 100)
  end

  test "typ validation" do
    assert verify_claim("typ", "bypass")
    assert verify_claim("typ", "firebase")
    refute verify_claim("typ", "foo")
    refute verify_claim("typ", nil)
  end
end
