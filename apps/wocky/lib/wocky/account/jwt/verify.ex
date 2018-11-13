defmodule Wocky.Account.JWT.Verify do
  @moduledoc "Basic claims verification for JWT tokens."

  use Guardian.Token.Verify

  alias Guardian.Token.Jwt.Verify, as: Base
  alias Guardian.Token.Verify

  @doc false
  def verify_claim(_mod, "iat", %{"iat" => nil} = claims, _opts),
    do: {:ok, claims}

  def verify_claim(mod, "iat", %{"iat" => iat} = claims, _opts) do
    if Verify.time_within_drift?(mod, iat) || iat <= Guardian.timestamp() do
      {:ok, claims}
    else
      {:error, :future_issue_date}
    end
  end

  def verify_claim(mod, claim_key, claims, opts),
    do: Base.verify_claim(mod, claim_key, claims, opts)
end
